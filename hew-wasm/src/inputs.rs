use wasm_bindgen::JsCast;

pub(crate) fn from_js(files: Option<js_sys::Map>) -> Result<hew_compile::SourceSet, String> {
    let mut inputs = hew_compile::SourceSet::in_memory();
    let Some(files) = files else {
        return Ok(inputs);
    };
    let entries = js_sys::try_iter(files.as_ref())
        .map_err(|_| "cannot read compilation inputs")?
        .ok_or("files must be a Map of paths to strings or Uint8Array values")?;
    for entry in entries {
        let pair = js_sys::Array::from(&entry.map_err(|_| "cannot read compilation input")?);
        let path = pair
            .get(0)
            .as_string()
            .ok_or("compilation input paths must be strings")?;
        if std::path::Path::new(&path).is_absolute() || path.split('/').any(|part| part == "..") {
            return Err("compilation input paths must stay inside the package".into());
        }
        let value = pair.get(1);
        if let Some(text) = value.as_string() {
            inputs.insert(path, text);
        } else if value.is_instance_of::<js_sys::Uint8Array>() {
            inputs.insert_bytes(path, js_sys::Uint8Array::new(&value).to_vec());
        } else {
            return Err("compilation inputs must be strings or Uint8Array values".into());
        }
    }
    Ok(inputs)
}
