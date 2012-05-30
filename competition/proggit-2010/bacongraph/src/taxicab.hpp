#ifndef TAXICAB_HPP_
#define TAXICAB_HPP_

// for abs
#include <cstdlib>

template <typename T>
T taxicab_distance(T a_x, T a_y, T b_x, T b_y)
{
    return abs(a_x - b_x) + abs(a_y - b_y);
}


#endif // TAXICAB_HPP_
