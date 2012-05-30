#ifndef SOLUTION_HPP_
#define SOLUTION_HPP_

#include <iostream>
#include <vector>
#include <utility> // for pair

class solution {
  public:
    solution()
      : total_distance_(0), bacon_coords_()
    {}
    void clear()
    { total_distance_ = 0; bacon_coords_.clear(); }

    bool init(const bacongraph& g)
    {
        return true;
    }
    template <typename charT, class traitsT>
     friend std::basic_ostream<charT, traitsT>& operator<<(std::basic_ostream<charT, traitsT>& os, const solution& s);

  private:
    template <typename charT, class traitsT>
    void print_total_distance(std::basic_ostream<charT, traitsT>& os) const;
    template <typename charT, class traitsT>
    void print_bacon_coords(std::basic_ostream<charT, traitsT>& os) const;

  public:
    typedef std::pair<int,int> coord_type;
    typedef std::vector<coord_type> coord_list_type;
  private:
    int total_distance_;
    coord_list_type bacon_coords_;
};
template <typename charT, class traitsT>
std::basic_ostream<charT, traitsT>& operator<<(std::basic_ostream<charT, traitsT>& os, const solution& s)
{
    os << "<solution:\n";
    s.print_total_distance(os);
    s.print_bacon_coords(os);
    os << ">\n";
    return os;
}

template <typename charT, class traitsT>
void solution::print_total_distance(std::basic_ostream<charT, traitsT>& os) const
{
    os << "<total_distance:" << total_distance_ << ">\n";
}

template <typename charT, class traitsT>
void solution::print_bacon_coords(std::basic_ostream<charT, traitsT>& os) const
{
    coord_list_type::const_iterator iter = bacon_coords_.begin();
    coord_list_type::const_iterator iter_end = bacon_coords_.end();

    os << "<bacon_coord_list:";
    for (; iter != iter_end; ++iter) {
        os << '(' << iter->first << ',' << iter->second << ")\n";
    }
    os << ">\n";
}

#endif // SOLUTION_HPP_
