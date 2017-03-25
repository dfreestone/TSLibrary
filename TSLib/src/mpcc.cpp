#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector mpcflushesc(NumericVector data, int nevents)
{
  NumericVector flush(nevents);

  int flush_start = 0;
  int flush_size = 0;
  int flush_end = nevents; // assume 1 chunk, change later.

  int flushsize_offset = 18;
  int datastart_offset = 24; // TODO(David): This is 23 for the h experiment, 24 for k. Why?

  int flushsize_index = flush_start + flushsize_offset;

  int flush_number = 1;
  for (int eventIndex=0; eventIndex<nevents; ++eventIndex)
  {
    flush[eventIndex] = flush_number;
    if (eventIndex == flushsize_index)
    {
      flush_size = datastart_offset + data[eventIndex];
      flush_end = flush_start + flush_size;
    }
    else if (eventIndex == flush_end)
    {
      ++flush_number;
      flush_start = 1 + eventIndex;
      flushsize_index = flush_start + flushsize_offset;
    }
  } // for
  return (flush);
} // function

// [[Rcpp::export]]
NumericMatrix mpc2tecc(NumericVector mpc, int nevents, double resolution)
{
  NumericMatrix tec(nevents, 2);

  int startIndex = 23; // start of data in each flush
  for (int eventIndex=startIndex; eventIndex<=nevents; ++eventIndex)
  {
    tec(eventIndex, 0) = resolution * int(mpc[eventIndex]);
    tec(eventIndex, 1) = round(1000 * (mpc[eventIndex] - int(mpc[eventIndex])));
  }
  return (tec);
}
