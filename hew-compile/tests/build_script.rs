mod build_script {
    #![allow(
        dead_code,
        reason = "build.rs helpers are exercised via included tests"
    )]

    include!("../build.rs");
}
