import os
from pathlib import Path

GIVEN_TEST_SUITE_ROOT_DIR = Path(os.environ["TEST"]) if "TEST" in os.environ else None

DEFAULT_TEST_SUITE_ROOT_DIR = Path(__file__).parent

TEST_SUITE_ROOT_DIR = (
    GIVEN_TEST_SUITE_ROOT_DIR
    if GIVEN_TEST_SUITE_ROOT_DIR is not None
    else DEFAULT_TEST_SUITE_ROOT_DIR
)

STELLA_FILE_SUFFIXES = {".stella", ".st"}

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
    # === Stage 1 ===
    "#unit-type",
    "#pairs",
    "#tuples",
    "#records",
    "#let-bindings",
    "#type-ascriptions",
    "#sum-types",
    "#lists",
    "#fixpoint-combinator",
    "#variants",
    # Optional extentions
    "#natural-literals",
    "#nullary-functions",
    "#multiparameter-functions",
    "#sequencing",
    # === Stage 2 ===
    "#references",
    "#panic",
    "#exceptions",
    "#exception-type-declaration",
    "#exception-type-annotation",
    "#structural-subtyping",
    "#ambiguous-type-as-bottom",
    "#top-type",
    "#bottom-type",
    # === Stage 3 ===
    "#type-reconstruction",
    "#universal-types",
}
