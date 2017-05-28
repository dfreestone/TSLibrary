#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector trialdefc(NumericVector events, NumericVector pattern,
                        int event_count, int pattern_count, bool from_first)
{

  NumericVector trial(event_count);
  NumericVector remove_code(pattern_count);

  // Input checking...
  if ((pattern[0] < 0) || (pattern[pattern_count] < 0))
  {
    return (trial);
  }

  int event_index = 0;
  int pattern_index = 0;
  int remove_index = -1;
  int trial_number = 0;
  int start_index = 0;
  while (event_index <= event_count)
  {
    // If you've found the start.
    if ((pattern_index==0) && (events[event_index]==pattern[0]))
    {
      ++pattern_index;
      remove_index = -1;
      start_index = event_index;
    }

    // If you've found the end.
    else if ((pattern_index==pattern_count) && (events[event_index]==pattern[pattern_count]))
    {
      pattern_index = 0;
      remove_index = -1;
      ++trial_number;
      for (int i=start_index; i<=event_index; ++i)
      {
        trial[i] = trial_number;
      } // for
    } // else

    // If you've found another event.
    else if (events[event_index]==pattern[pattern_index])
    {
      ++pattern_index;
      remove_index = -1;
    } // else


    // If you've found a new start you weren't looking for
    else if ((!from_first) && (pattern_index > 0) && (events[event_index]==pattern[0]))
    {
      pattern_index = 0;
      --event_index;
    }

    // If you're looking for remove values
    else if (remove_index > 0)
    {
      for (int i=0; i<=remove_index; ++i)
      {
        if (events[event_index] == remove_code[i])
        {
          event_index = start_index;
          break;
        } // if
      } // for
    } // else

    // If you've found a remove code, keep searching...
    if (pattern[pattern_index] < 0)
    {
      while (pattern[pattern_index] < 0)
      {
        ++remove_index;
        remove_code[remove_index] = abs(int(pattern[pattern_index]));
        ++pattern_index;
      } // while
    } // if
    ++event_index;
  } // while
  return trial;
}
