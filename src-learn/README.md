This folder holds code to analyse doors project data using the approach in [Maggi et al. (2024)](https://doi.org/10.7554/eLife.86491). They count each action by a participant as evidence for or against (1 or 0) each of a list of pre-defined strategies. Each count of evidence updates a beta distribution that tracks the probability that the strategy is in play. The mode and variance of the beta distribution show us at each evaluation point (trial, click, decision) how well the strategy explains the behaviour. Evidence is recency-weighted, so that the algorithm can also detect when a strategy drops off or gains dominance. 

We are using this to find the moment at which participants grasp which doors are relevant for the current context. In our case, we estimate what people know, rather than what strategy they are following. We track whether their choices are consistent with them knowing x. Specifically, we track whether their choices are consistent with them knowing 1, 2, 3, or all 4 doors of a context. If someone has seen a target behind a door, and they choose that door before they choose any incorrect doors, we count that as a deliberate click on the context-relevant door. We apply this reasoning both within each trial and with a sliding window of four clicks. In both cases, we count how many unique context-relevant doors they click before they click a context-irrelevant door.

#### Files

| file                    | notes                                                        |
| ----------------------- | ------------------------------------------------------------ |
| synthesised_data.csv    | Data that we synthesised to test our own implementation of the Maggi algorithm against theirs. |
| maggi_py_results.csv    | Results produced by Maggi et al.'s Python implementation of their algorithm. We used the data in synthesised_data.csv. |
| cf_maggi_py.R           | Our test implementation of Maggi's algorithm. We also create the synthesised data here. |
| run_maggi.R             | A wrapper script to apply Maggi's algorithm to our doors task data. It calls format_data_for_maggi.R and get_maggi.R, applies a learning rule to the Maggi data to detect change points, then saves and plots the output. |
| format_data_for_maggi.R | This function loads doors task data and tracks evidence for our "know-n" strategies. It includes variations that make the strategies independent or competitive, evaluate them on trials or a sliding window, and link "know-n" to specific doors or just the number of known doors. |
| get_maggi.R             | This function takes evidence for each strategy, as a vector of ones and zeros, and generates a beta distribution from that evidence. The distribution updates for each new piece of evidence. We extract the parameters of that beta distribution at each evaluation point to get a timecourse of evidence for the strategy. |



