"""
This module analyses the data for the study Too Tired to Know.
"""

from abc import ABC

import numpy as np
import pandas as pd


class Test(ABC):
    """
    Represents one of the tests given to the participants. There were five such tests in total.
    """
    def __init__(self, path: str) -> None:
        self.path = path
        self.data = pd.read_csv(path)
        return


class Arithmetic(Test):
    """
    The arithmetic test.

    The columns in the data file are:
    - t0: Can be ignored
    - order_of_test: Order of the math test in the test battery
    - time_start_battery: Time from opening of the app until response (I think)
    - order_in_test: Order for each math questions in the current test
    - base1: The first number to be added with the second
    - base2: The second number to be added with the first
    - answer_m: Participant's response
    - time_m: Response time
    - correct: Whether the participant's response was correct
    - ID: Participant ID
    - time: Session number (i.e. order of test battery)
    - t0_absolute: Can be ignored
    - Woman: Participant gender
    - SD: Participant sleep status

    """
    def __init__(self, path: str) -> None:
        Test.__init__(self, path)
        return


class EpisodicMemory(Test):
    """

    The columns in the data file are:
    - t0: This is irrelevant
    - order_of_test: Order of the math test in the test battery
    - time_start_battery: Time from opening of the app until response (I think)
    - order_in_test: Order for each STM round in the current test
    - word_pool: Which word pool has been used
    - word: Order of the presented word in the test
    - endcoding	[sic]: Whether the presented word was shown during the encoding phase or not
    - recall_session: Which recall session (i.e. the same info as "order_in_test)
    - remember: Whether the participant indicated remembering the word from the encoding
    - confidence: How certain the participant is of their response being correct
    - correct: Whether the participant response was correct
    - ID: Participant ID
    - time: Session number (i.e. order of test battery)
    - t0_absolute: N/A
    - Woman: Participant gender
    - SD: Participant sleep status
    """
    def __init__(self, path: str) -> None:
        Test.__init__(self, path)
        return


class SimpleAttention(Test):
    """

    The columns in the data file are:
    - t0: This is irrelevant
    - order_of_test: Order of the math test in the test battery
    - time_start_battery: Time from opening of the app until response (I think)
    - order_in_test: Order for each reaction time round in the current test
    - stimuli_type: The symbol that was shown for the participants to react
    - response: Whether the participant pressed the button in response to the stimulus
    - reaction_time: Reaction time from the appearance of stimulus until pressing the button
    - false_responses: Indicates that the participant pressed the button when there was no stimulus
    - average_reaction_time: Average reaction time within session
    - median_reaction_time: Median rection time within session
    - reaction_time_standard_deviation: Standard deviation of the average reaction time within session
    - ID: Participant ID
    - time: Session number (i.e. order of test battery)
    - t0_absolute: This is irrelevant
    - Woman: Participant gender
    - SD: Participant sleep status
    - lapse500: Whether the reaction time was >500ms
    - lapse600: Whether the reaction time was >600ms
    - lapse700: Whether the reaction time was >700ms
    - lapse800: Whether the reaction time was >800ms
    - lapse900: Whether the reaction time was >900ms
    - lapse1000: Whether the reaction time was >1000ms
    - correct: This is irrelevant
    """
    def __init__(self, path: str) -> None:
        Test.__init__(self, path)
        return


class Stroop(Test):
    """

    The columns in the data file are:
    - t0: This is irrelevant
    - order_of_test: Order of the math test in the test battery
    - time_start_battery: Time from opening of the app until response (I think)
    - order_in_test: Order for each stroop trial in the current test
    - color: Colour of the font
    - text: The word that was shown
    - response: The colour that the participant responded
    - reaction_time: Reaction time of the response
    - correct: Whether the response was correct (i.e. the colour of the font)
    - congruent: Whether the trial was congruent, i.e. the font colour was the same as the word
    - stimuli_combination: The combination of the current trial with the previous trial
    - ID: Participant ID
    - time: Session number (i.e. order of test battery)
    - t0_absolute: N/A
    - Woman: Participant gender
    - SD: Participant sleep status
    - cognitive_update: Whether there was a difference between the previous and current trial types (i.e. there needed to be a cognitive update)
    - ERROR: Whether the participant made an error, i.e. the reverse of "correct"
    """
    def __init__(self, path: str) -> None:
        Test.__init__(self, path)
        return


class WorkingMemory(Test):
    """

    The columns in the data file are:
    - t0: This is irrelevant
    - order_of_test: Order of the math test in the test battery
    - time_start_battery: Time from opening of the app until response (I think)
    - order_in_test: Order for each reaction time round in the current test
    - order: Order of grid presented
    - no_grids: Number of grids that turned red before asking for a response
    - grid1: Which was the first grid to turn red
    - grid2: Which was the second grid to turn red
    - grid3: Which was the third grid to turn red
    - grid4: Which was the fourth grid to turn red
    - grid5: Which was the fifth grid to turn red
    - grid6: Which was the sixth grid to turn red
    - grid7: Which was the seventh grid to turn red
    - grid8: Which was the eighth grid to turn red
    - pres_grid: Indicates which grid is asked about, i.e. "was this grid the 4th to turn red?"
    - correct: Whether participant's response was correct
    - response_time: Response time from showing the question to clicking yes or no
    - ID: Participant ID
    - time: Session number (i.e. order of test battery)
    - t0_absolute: N/A
    - Woman: Participant gender
    - SD: Participant sleep status
    """
    def __init__(self, path: str) -> None:
        Test.__init__(self, path)
        return
