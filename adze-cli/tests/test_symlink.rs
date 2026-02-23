use std::fs;
use std::io::Read;
use tar::Header;
use tempfile::TempDir;

#[test]
fn test_tar_symlink_extraction() {
    // Create a tar archive with a symlink entry
    let mut tar_data = Vec::new();
    {
        let mut builder = tar::Builder::new(&mut tar_data);

        // Create a symlink: "link" points to "../../outside"
        let mut symlink_header = Header::new_ustar();
        symlink_header.set_entry_type(tar::EntryType::Symlink);
        symlink_header.set_size(0);
        symlink_header.set_mode(0o777);
        symlink_header.set_uid(0);
        symlink_header.set_gid(0);
        symlink_header.set_mtime(0);
        symlink_header.set_cksum();

        builder
            .append_link(&mut symlink_header, "link", "../../outside")
            .unwrap();

        // Add a regular file after it
        let mut file_header = Header::new_ustar();
        file_header.set_size(5);
        file_header.set_mode(0o644);
        file_header.set_uid(0);
        file_header.set_gid(0);
        file_header.set_mtime(0);
        file_header.set_cksum();

        builder
            .append_data(&mut file_header, "regular.txt", &b"hello"[..])
            .unwrap();
        builder.finish().unwrap();
    }

    // Now examine the tar entries
    let mut archive = tar::Archive::new(tar_data.as_slice());
    let mut symlink_found = false;
    let mut symlink_path = String::new();
    let mut symlink_target = String::new();

    for entry in archive.entries().unwrap() {
        let mut entry = entry.unwrap();
        let path = entry.path().unwrap().into_owned();
        let entry_type = entry.header().entry_type();

        println!("Entry: {:?}, Type: {:?}", path, entry_type);

        if entry_type.is_symlink() {
            symlink_found = true;
            symlink_path = path.to_string_lossy().to_string();

            // Try to get link target
            if let Ok(Some(target)) = entry.link_name() {
                symlink_target = target.to_string_lossy().to_string();
                println!("  Symlink target: {:?}", symlink_target);
            }

            // What does read_to_end return for a symlink?
            let mut contents = Vec::new();
            entry.read_to_end(&mut contents).unwrap();
            println!(
                "  read_to_end() returned {} bytes: {:?}",
                contents.len(),
                String::from_utf8_lossy(&contents)
            );
        }
    }

    assert!(symlink_found, "Symlink entry should be in the archive");
    assert_eq!(symlink_path, "link");
    assert_eq!(symlink_target, "../../outside");
}

#[test]
fn test_unpack_does_not_check_symlinks() {
    // Create a tar with a symlink pointing outside
    let mut tar_data = Vec::new();
    {
        let mut builder = tar::Builder::new(&mut tar_data);

        // Symlink "bad_link" -> "../../../../etc/passwd"
        let mut symlink_header = Header::new_ustar();
        symlink_header.set_entry_type(tar::EntryType::Symlink);
        symlink_header.set_size(0);
        symlink_header.set_mode(0o777);
        symlink_header.set_uid(0);
        symlink_header.set_gid(0);
        symlink_header.set_mtime(0);
        symlink_header.set_cksum();

        builder
            .append_link(&mut symlink_header, "bad_link", "../../../../etc/passwd")
            .unwrap();
        builder.finish().unwrap();
    }

    // Compress
    let compressed = zstd::encode_all(tar_data.as_slice(), 22).unwrap();

    // Try to unpack with the current adze-cli code
    let temp = TempDir::new().unwrap();
    let target = temp.path().join("extracted");

    let decompressed = zstd::decode_all(compressed.as_slice()).unwrap();
    let mut archive = tar::Archive::new(decompressed.as_slice());

    fs::create_dir_all(&target).unwrap();

    for entry in archive.entries().unwrap() {
        let mut entry = entry.unwrap();
        let path = entry.path().unwrap().into_owned();
        let entry_type = entry.header().entry_type();

        println!("Processing: {:?}, type: {:?}", path, entry_type);

        // Current code only checks the path, not the type
        if path.is_absolute()
            || path
                .components()
                .any(|c| c == std::path::Component::ParentDir)
        {
            println!("  -> Skipped by path check");
            continue;
        }

        // This code WOULD execute for the symlink
        let target_path = target.join(&path);
        println!("  -> Would write to: {:?}", target_path);

        // The symlink entry has no data - read_to_end returns 0 bytes
        let mut contents = Vec::new();
        entry.read_to_end(&mut contents).unwrap();

        if !contents.is_empty() {
            println!("  -> Would write {} bytes", contents.len());
            fs::write(&target_path, &contents).unwrap();
        }
    }

    // Check what was created
    let link_path = target.join("bad_link");
    if link_path.exists() {
        println!("File 'bad_link' exists after unpack");
        if link_path.is_symlink() {
            println!("  -> It's a symlink!");
        } else {
            let contents = fs::read_to_string(&link_path).unwrap_or_default();
            println!("  -> It's a regular file with {} bytes", contents.len());
        }
    } else {
        println!("File 'bad_link' does NOT exist after unpack");
    }
}
