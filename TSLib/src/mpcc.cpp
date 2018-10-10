#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector mpcflushesc(NumericVector data, int nevents)
{
  NumericVector flush(nevents);

  int flush_start = 0;
  int flush_size = 0;
  int flush_end = nevents; // assume 1 chunk, change later.

  int flushsize_offset = 17;
  int flushsize_index = flush_start + flushsize_offset;

  int flush_number = 1;

  for (int eventIndex=0; eventIndex<nevents; ++eventIndex)
  {
    flush[eventIndex] = flush_number;
    if (eventIndex == flushsize_index)
    {
      // data[eventIndex] gives the number of arrays in the program
      // data[eventIndex+1] gives the number of elements in the A array
      //                    which is the only one written to disk
      // +1 because we have to skip over eventIndex and eventIndex+1
      flush_size = flushsize_offset + data[eventIndex] + data[eventIndex+1];
      flush_end = flush_start + flush_size;
    }
    else if (eventIndex == flush_end)
    {

      // The next event should be the next flush. But if the array wasn't sealed
      //    the next event(s) will be a series of zeros. There's nothing in the
      //    data file indicating how many there will be. So find the end, and
      //    repark the eventIndex there.
      if (data[1+flush_end] == 0) {++flush_end; continue;}

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
