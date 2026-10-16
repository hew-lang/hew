//! Windows identity files use protected current-user DACLs and opened-handle checks.

use super::{invalid, File, Path};
use std::ffi::c_void;
use std::io;
use std::os::windows::ffi::OsStrExt;
use std::os::windows::fs::MetadataExt;
use std::os::windows::io::{AsRawHandle, FromRawHandle};
use std::ptr;
use windows_sys::Win32::Foundation::{CloseHandle, LocalFree, HANDLE, INVALID_HANDLE_VALUE};
use windows_sys::Win32::Security::Authorization::{
    ConvertSidToStringSidW, ConvertStringSecurityDescriptorToSecurityDescriptorW, GetSecurityInfo,
    SDDL_REVISION_1, SE_FILE_OBJECT,
};
use windows_sys::Win32::Security::{
    EqualSid, GetAce, GetSecurityDescriptorControl, GetTokenInformation, IsWellKnownSid, TokenUser,
    WinCreatorOwnerRightsSid, ACCESS_ALLOWED_ACE, ACL, DACL_SECURITY_INFORMATION,
    OWNER_SECURITY_INFORMATION, SECURITY_ATTRIBUTES, SE_DACL_PROTECTED, TOKEN_QUERY, TOKEN_USER,
};
use windows_sys::Win32::Storage::FileSystem::{
    CreateFileW, GetFileType, MoveFileExW, CREATE_NEW, FILE_ATTRIBUTE_NORMAL,
    FILE_ATTRIBUTE_REPARSE_POINT, FILE_FLAG_BACKUP_SEMANTICS, FILE_FLAG_OPEN_REPARSE_POINT,
    FILE_GENERIC_READ, FILE_GENERIC_WRITE, FILE_SHARE_DELETE, FILE_SHARE_READ, FILE_SHARE_WRITE,
    FILE_TYPE_DISK, MOVEFILE_WRITE_THROUGH, OPEN_EXISTING,
};
use windows_sys::Win32::System::Threading::{GetCurrentProcess, OpenProcessToken};

struct Local(*mut c_void);
impl Drop for Local {
    fn drop(&mut self) {
        if !self.0.is_null() {
            // SAFETY: owned allocation returned by the Windows security API.
            unsafe { LocalFree(self.0) };
        }
    }
}

struct Token(HANDLE);
impl Drop for Token {
    fn drop(&mut self) {
        // SAFETY: this token is owned and has not been transferred.
        unsafe { CloseHandle(self.0) };
    }
}

struct User(Vec<usize>);
impl User {
    fn current() -> io::Result<Self> {
        let mut token = ptr::null_mut();
        // SAFETY: writable output and a valid process pseudo-handle.
        if unsafe { OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &raw mut token) } == 0 {
            return Err(io::Error::last_os_error());
        }
        let token = Token(token);
        let mut length = 0;
        // SAFETY: null buffer requests its required size.
        unsafe { GetTokenInformation(token.0, TokenUser, ptr::null_mut(), 0, &raw mut length) };
        if length == 0 {
            return Err(io::Error::last_os_error());
        }
        let mut user = Self(vec![0; (length as usize).div_ceil(size_of::<usize>())]);
        // SAFETY: the aligned buffer has at least the requested byte capacity.
        if unsafe {
            GetTokenInformation(
                token.0,
                TokenUser,
                user.0.as_mut_ptr().cast(),
                length,
                &raw mut length,
            )
        } == 0
        {
            return Err(io::Error::last_os_error());
        }
        Ok(user)
    }

    fn sid(&self) -> *mut c_void {
        // SAFETY: GetTokenInformation initialized this aligned TOKEN_USER buffer.
        unsafe { (*self.0.as_ptr().cast::<TOKEN_USER>()).User.Sid }
    }

    fn descriptor(&self) -> io::Result<Local> {
        let mut encoded = ptr::null_mut();
        // SAFETY: the SID lives in this user's initialized buffer.
        if unsafe { ConvertSidToStringSidW(self.sid(), &raw mut encoded) } == 0 {
            return Err(io::Error::last_os_error());
        }
        let encoded = Local(encoded.cast());
        let chars = encoded.0.cast::<u16>();
        let mut length = 0;
        // SAFETY: the Windows API returns a null-terminated SID string.
        unsafe {
            while *chars.add(length) != 0 {
                length += 1;
            }
        }
        // SAFETY: the scan above found the initialized string's terminator.
        let sid = String::from_utf16(unsafe { std::slice::from_raw_parts(chars, length) })
            .map_err(|_| invalid("invalid current-user SID"))?;
        let sddl: Vec<u16> = format!("O:{sid}D:P(A;;FA;;;{sid})")
            .encode_utf16()
            .chain([0])
            .collect();
        let mut descriptor = ptr::null_mut();
        // SAFETY: SDDL is terminated and the output slot is writable.
        if unsafe {
            ConvertStringSecurityDescriptorToSecurityDescriptorW(
                sddl.as_ptr(),
                SDDL_REVISION_1,
                &raw mut descriptor,
                ptr::null_mut(),
            )
        } == 0
        {
            return Err(io::Error::last_os_error());
        }
        Ok(Local(descriptor))
    }
}

pub(super) fn open(path: &Path, create: bool) -> io::Result<File> {
    let user = User::current()?;
    let descriptor = user.descriptor()?;
    let attributes = SECURITY_ATTRIBUTES {
        nLength: u32::try_from(size_of::<SECURITY_ATTRIBUTES>()).unwrap(),
        lpSecurityDescriptor: descriptor.0,
        bInheritHandle: 0,
    };
    let mut path_wide: Vec<u16> = path.as_os_str().encode_wide().collect();
    if path_wide.contains(&0) {
        return Err(invalid("identity path contains NUL"));
    }
    path_wide.push(0);
    // SAFETY: path and descriptor are live through creation; inheritance is off.
    let handle = unsafe {
        CreateFileW(
            path_wide.as_ptr(),
            FILE_GENERIC_READ | if create { FILE_GENERIC_WRITE } else { 0 },
            FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
            &raw const attributes,
            if create { CREATE_NEW } else { OPEN_EXISTING },
            FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS,
            ptr::null_mut(),
        )
    };
    if handle == INVALID_HANDLE_VALUE {
        return Err(io::Error::last_os_error());
    }
    // SAFETY: this newly opened handle is uniquely owned by the File.
    let file = unsafe { File::from_raw_handle(handle) };
    validate(&file)?;
    if create {
        file.lock()?;
    } else {
        file.lock_shared()?;
    }
    Ok(file)
}

pub(super) fn validate(file: &File) -> io::Result<()> {
    // SAFETY: the File owns this live handle. Pipes and devices are not keys.
    if unsafe { GetFileType(file.as_raw_handle()) } != FILE_TYPE_DISK {
        return Err(invalid("identity must be a disk file"));
    }
    let metadata = file.metadata()?;
    if !metadata.is_file() || metadata.file_attributes() & FILE_ATTRIBUTE_REPARSE_POINT != 0 {
        return Err(invalid(
            "identity must be a regular file, never a reparse point",
        ));
    }
    let user = User::current()?;
    let mut owner = ptr::null_mut();
    let mut dacl: *mut ACL = ptr::null_mut();
    let mut descriptor = ptr::null_mut();
    // SAFETY: the file handle is live and all requested outputs are writable.
    let status = unsafe {
        GetSecurityInfo(
            file.as_raw_handle(),
            SE_FILE_OBJECT,
            OWNER_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION,
            &raw mut owner,
            ptr::null_mut(),
            &raw mut dacl,
            ptr::null_mut(),
            &raw mut descriptor,
        )
    };
    if status != 0 {
        return Err(io::Error::from_raw_os_error(
            i32::try_from(status).unwrap_or(i32::MAX),
        ));
    }
    let descriptor = Local(descriptor);
    let mut control = 0;
    let mut revision = 0;
    // SAFETY: the OS supplied descriptor and owner SID remain owned above.
    let current_owner = unsafe { EqualSid(owner, user.sid()) } != 0;
    // SAFETY: the descriptor is live and both control outputs are writable.
    let has_control =
        unsafe { GetSecurityDescriptorControl(descriptor.0, &raw mut control, &raw mut revision) }
            != 0;
    if !current_owner || dacl.is_null() || !has_control || control & SE_DACL_PROTECTED == 0 {
        return Err(invalid(
            "identity requires a protected current-user-only DACL",
        ));
    }
    // SAFETY: GetSecurityInfo returned the valid ACL inside this descriptor.
    for index in 0..unsafe { (*dacl).AceCount } {
        let mut ace = ptr::null_mut();
        // SAFETY: the index is within the OS-supplied ACL's ACE count.
        if unsafe { GetAce(dacl, u32::from(index), &raw mut ace) } == 0 {
            return Err(io::Error::last_os_error());
        }
        let allowed = ace.cast::<ACCESS_ALLOWED_ACE>();
        // SAFETY: each ACE starts with ACE_HEADER. Type zero is ACCESS_ALLOWED_ACE.
        if unsafe { (*allowed).Header.AceType } != 0 {
            return Err(invalid(
                "identity DACL contains an unsupported access entry",
            ));
        }
        // SAFETY: the OS validates the ACE; SidStart begins its variable SID.
        let sid = unsafe { (&raw const (*allowed).SidStart).cast_mut().cast() };
        // SAFETY: both SIDs are initialized within the OS-supplied descriptor.
        let matches_owner = unsafe { EqualSid(sid, owner) } != 0;
        // SAFETY: the SID is initialized within the OS-supplied ACE.
        let owner_rights = unsafe { IsWellKnownSid(sid, WinCreatorOwnerRightsSid) } != 0;
        if !matches_owner && !owner_rights {
            return Err(invalid("identity DACL grants access beyond its owner"));
        }
    }
    Ok(())
}

pub(super) fn publish(temporary: tempfile::NamedTempFile, path: &Path) -> io::Result<File> {
    let (file, mut temporary_path) = temporary.into_parts();
    let from: Vec<u16> = temporary_path
        .as_os_str()
        .encode_wide()
        .chain([0])
        .collect();
    let to: Vec<u16> = path.as_os_str().encode_wide().chain([0]).collect();
    // SAFETY: both paths are terminated. No REPLACE_EXISTING flag is supplied;
    // WRITE_THROUGH commits the move before reporting successful publication.
    if unsafe { MoveFileExW(from.as_ptr(), to.as_ptr(), MOVEFILE_WRITE_THROUGH) } == 0 {
        return Err(io::Error::last_os_error());
    }
    temporary_path.disable_cleanup(true);
    Ok(file)
}
