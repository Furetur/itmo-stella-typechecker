from pathlib import Path


TEST_SUITE_ROOT_DIR = Path(__file__).parent

STELLA_FILE_SUFFIXES = { ".stella", ".st" }

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
    "#unit-type",
    "#pairs",
    "#tuples",
    "#records",
    "#let-bindings",
    "#type-ascriptions",
    "#sum-types",
    "#lists",
    "#fixpoint-combinator",
    "#variants"
    # Optional extentions
    "#natural-literals",
    "#nullary-functions",
    "#multiparameter-functions",
    "#sequencing",
}
