"""
This module analyses the data for the study Too Tired to Know.
"""


import numpy as np
import pandas as pd


class Test:
    """
    Represents one of the tests given to the participants. There were five such tests in total.
    """
    def __init__(self, path: str) -> None:
        self.path = path
        return


class ArithmeticTest(Test):
    def __init__(self, path: str) -> None:
        Test.__init__(self, path)
        return


class StroopTest(Test):
    def __init__(self, path: str) -> None:
        Test.__init__(self, path)
        return


class WorkingMemoryTest(Test):
    def __init__(self, path: str) -> None:
        Test.__init__(self, path)
        return


class EpisodicMemoryTest(Test):
    def __init__(self, path: str) -> None:
        Test.__init__(self, path)
        return


class SimpleAttentionTest(Test):
    def __init__(self, path: str) -> None:
        Test.__init__(self, path)
        return
