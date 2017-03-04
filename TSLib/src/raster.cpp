#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rasterc(NumericVector events, NumericVector pattern, int event_count)
{
  NumericVector raster(event_count);
  int oncode = pattern[0];
  int offcode = pattern[1];
  int stopcode = pattern[2];

  int state = 0;
  int response_number = 0;
  for (int event_index=0; event_index<=event_count; ++event_index)
  {
    if ((state==0) && (events[event_index]==oncode))
    {
      ++state;
      raster[event_index] = ++response_number;
    }
    else if ((state==1) &&
             ((events[event_index]==offcode) || (events[event_index]==stopcode)))
    {
      state = 0;
      raster[event_index] = response_number;
    }
  }
  return(raster);
}
