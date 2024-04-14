from typing import Tuple
import pytest
from .discover_tests import discover_tests, SingleTest, get_pytest_test_id, pytest_discover_tests
from .run_each import run_test


# Entry points


@pytest.mark.parametrize("single_test,", discover_tests(), ids=get_pytest_test_id)
def test_exec(single_test: SingleTest):
    run_test(single_test)

