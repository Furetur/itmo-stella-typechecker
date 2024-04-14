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
    "#natural-literals",
    "#tuples",
    "#unit-type",
    "#pairs",
    "#let-bindings",
    "#nullary-functions",
    "#multiparameter-functions",
    "#fixpoint-combinator",
    "#sum-types",
    "#lists",
    "#records",
    "#panic",
    "#sequencing",
    "#references",
    "#exceptions",
    "#exception-type-declaration"
}
