"""
When finished, this script will run the entire statistical analysis for the paper Too Tired to Know?
"""

import Analysis

arit = Analysis.Arithmetic("Data/arithmetic_data.csv")

ep = Analysis.EpisodicMemory("Data/episodic_memory_data.csv")

simp = Analysis.SimpleAttention("Data/simple_attention_data.csv")

stroop = Analysis.Stroop("Data/Stroop_data.csv")

work = Analysis.WorkingMemory("Data/working_memory_data.csv")