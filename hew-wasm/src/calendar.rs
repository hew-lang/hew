//! Narrow sandbox binding to the native calendar engine.

use hew_calendar::Error;
use serde::Deserialize;
use serde_json::{json, Value};
use wasm_bindgen::prelude::*;

#[derive(Deserialize)]
struct Request {
    operation: String,
    args: Vec<String>,
}

/// Execute a pure checked calendar operation. Integers cross JSON as decimal
/// strings so the timestamp's full seconds range never passes through f64.
#[wasm_bindgen(js_name = executeCalendar)]
#[must_use]
pub fn execute_calendar(request: &str) -> String {
    let result = serde_json::from_str::<Request>(request)
        .map_err(|error| Error::Parse(error.to_string()))
        .and_then(execute);
    match result {
        Ok(value) => value,
        Err(error) => json!({"status": error.code(), "message": error.to_string()}),
    }
    .to_string()
}

fn execute(request: Request) -> Result<Value, Error> {
    let arg = |index: usize| {
        request
            .args
            .get(index)
            .map(String::as_str)
            .ok_or_else(|| Error::Parse("calendar argument is missing".into()))
    };
    let integer = |index| {
        arg(index)?
            .parse::<i64>()
            .map_err(|_| Error::Parse("calendar integer is invalid".into()))
    };
    let nanos = |index| {
        arg(index)?
            .parse::<u32>()
            .map_err(|_| Error::Parse("calendar nanoseconds are invalid".into()))
    };
    match request.operation.as_str() {
        "format" => Ok(json!({
            "status": 0, "message": "",
            "text": hew_calendar::format(integer(0)?, nanos(1)?, arg(2)?)?,
        })),
        "parse" => hew_calendar::parse(arg(0)?, arg(1)?).map(time_value),
        "from_parts" => hew_calendar::from_parts(
            integer(0)?,
            integer(1)?,
            integer(2)?,
            integer(3)?,
            integer(4)?,
            integer(5)?,
            nanos(6)?,
        )
        .map(time_value),
        "components" => {
            let value = hew_calendar::components(integer(0)?, nanos(1)?)?;
            Ok(json!({
                "status": 0, "message": "", "year": value.year.to_string(),
                "month": value.month.to_string(), "day": value.day.to_string(),
                "hour": value.hour.to_string(), "minute": value.minute.to_string(),
                "second": value.second.to_string(), "nanosecond": value.nanosecond.to_string(),
                "weekday": value.weekday.to_string(), "days_in_month": value.days_in_month.to_string(),
            }))
        }
        _ => Err(Error::Parse("unknown calendar operation".into())),
    }
}

fn time_value((seconds, nanoseconds): (i64, u32)) -> Value {
    json!({"status": 0, "message": "", "seconds": seconds.to_string(), "nanoseconds": nanoseconds.to_string()})
}
