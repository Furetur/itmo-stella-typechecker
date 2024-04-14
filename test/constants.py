from pathlib import Path


OK_TESTS_DIR = Path(__file__).parent / "stella-tests" / "ok"

BAD_TESTS_DIR = Path(__file__).parent / "stella-tests" / "bad"

DEFAULT_EXECUTABLE_PATH = (
    Path(__file__).parent.parent / "_build" / "default" / "bin" / "main.exe"
)

EXTENTION_REGEX = r"#[a-z\-]+"

ERROR_REGEX = r"ERROR_[A-Z_]+"
ERROR_NOT_IMPLEMENTED = "ERROR_NOT_IMPLEMENTED"
ERROR_PARSE_ERROR = "PARSE_ERROR"

REQUIRED_EXTENTIONS = {
    "#panic",
    "#natural-literals",
    "#tuples",
    "#unit-type",
    "#pairs",
    "#let-bindings",
    "#nullary-functions",
    "#sum-types",
    "#lists",
    "#records"

}
