#ifndef BACONGRAPH_HPP_
#define BACONGRAPH_HPP_

#include <iostream>
#include <vector>
#include <algorithm>

#include <cassert>

// contains the parsed representation of the city map and header (bacon number
// etc.)
class bacongraph {
  public:
    bacongraph()
      :  rows_(0), cols_(0), city_rows_()
    {}
    void clear() { rows_ = cols_ = 0; city_rows_.clear(); }

    // initialize with an input stream
    template <typename charT, class traitsT>
    bool init(std::basic_istream<charT, traitsT>& in);
    // same as init(istream&). to check for error, check failbit on the stream
    template <typename charT, class traitsT>
     friend std::basic_istream<charT, traitsT>& operator>>(std::basic_istream<charT, traitsT>& os, const bacongraph& b);

    // outputs the map in the same format as the source, not very useful
    template <typename charT, class traitsT>
     friend std::basic_ostream<charT, traitsT>& operator<<(std::basic_ostream<charT, traitsT>& os, const bacongraph& b);

  private:
    // does the real work of init
    template <typename charT, class traitsT>
    bool init__(std::basic_istream<charT, traitsT>& in);

    // we store the cities sparsely, keeping a list of rows, which contain a
    // list of cities
    struct row {
        typedef std::vector<int> city_cols_type;

        row(int rr, int cc)
          : row_index(rr), city_cols()
        { city_cols.push_back(cc); }

        // the row index, starts from 0
        int row_index;
        city_cols_type city_cols;

        // predicate useful when trying to find a bacongraph::row with a
        // particular row_index, e.g., with std::find_if
        struct row_pred {
            row_pred(int row_to_be_found)
              : row_to_be_found_(row_to_be_found)
            {}
            bool operator()(const row& g) const
            { return g.row_index == row_to_be_found_; }

            int row_to_be_found_;
        };
    };
    // used in operator<< to print a single row (can't be an operator<< for
    // bacongraph::row as it needs the cols_ count
    void print_row(std::ostream& os, const row& rr) const;
  public:
    typedef std::vector<row> row_list_type;
  private:
    int rows_;
    int cols_;
    int bacons_;
    row_list_type city_rows_;
};

void bacongraph::print_row(std::ostream& os, const row& rr) const
{
    row::city_cols_type::const_iterator city = rr.city_cols.begin();
    assert(city != rr.city_cols.end());
    int cur_col = 0;
    for (; city != rr.city_cols.end(); ++city) {
        // fill in blanks before city (if any)
        for (; cur_col < *city; ++cur_col) {
            os << '.';
        }
        // print city and incr cur_col
        os << 'P';
        ++cur_col;
    }
    for (; cur_col < rows_; ++cur_col) {
        os << '.';
    }
    os << '\n';
}


namespace {
template <typename charT, class traitsT>
static std::basic_ostream<charT, traitsT>& print_empty_row(std::basic_ostream<charT, traitsT>& os, int cols)
{
    for (int c = 0; c < cols; ++c) {
        os << '.';
    }
    os << '\n';
}
} // namespace ''

template <typename charT, class traitsT>
std::basic_ostream<charT, traitsT>& operator<<(std::basic_ostream<charT, traitsT>& os, const bacongraph& b)
{
    os << b.cols_ << 'x' << b.rows_ << ' ' << b.bacons_ << '\n';
    bacongraph::row_list_type::const_iterator row = b.city_rows_.begin();
    int cur_row = 0;
    if (row != b.city_rows_.end()) {
        for (; row != b.city_rows_.end(); ++row) {
            // fill in blank rows up to current row
            for (; cur_row < row->row_index; ++cur_row) {
                print_empty_row(os, b.cols_);
            }
            // print the row and incr cur_row
            b.print_row(os, *row);
            cur_row = row->row_index + 1;
        }
        // print any remaining empty rows
        for (; cur_row < b.rows_; ++cur_row) {
            print_empty_row(os, b.cols_);
        }
    } else {
        // no cities
        for (int r = 0; r < b.rows_; ++r) {
            print_empty_row(os, b.cols_);
        }
    }
}
template <typename charT, class traitsT>
std::basic_istream<charT, traitsT>& operator>>(std::basic_istream<charT, traitsT>& os, bacongraph& b)
{
    b.init(os);
}

template <typename charT, typename traitsT>
struct noskipws {
    noskipws(std::basic_istream<charT, traitsT>& in)
      : flags_(), in_(in)
    {
        flags_ = in_.flags();
        if (flags_ & std::ios::skipws) {
            in_.unsetf(std::ios::skipws);
        }
    }
    ~noskipws()
    {
        if (flags_ & std::ios::skipws) {
            in_.setf(std::ios::skipws);
        }
    }
    std::ios::fmtflags flags_;
    std::basic_istream<charT, traitsT>& in_;
};

template <typename charT, typename traitsT>
bool bacongraph::init(std::basic_istream<charT, traitsT>& in)
{
    clear();
    bool b = init__(in);
    if (!b) {
        in.setstate(std::ios::failbit);
    } else {
        in.clear( in.rdstate() & ~std::ios::failbit );
    }
    return b;
}
template <typename charT, typename traitsT>
bool bacongraph::init__(std::basic_istream<charT, traitsT>& in)
{
    /* test stream ok */
    if (!in) {
        std::cerr << "input stream is bad!\n";
        return false;
    }
    /* make sure skipws not set */
    noskipws<charT, traitsT> ws(in);

    /* get cols */
    in >> cols_;
    if (!in) {
        std::cerr << "could not read columns!\n";
        return false;
    }
    if (cols_ <= 0) {
        std::cerr << "invalid cols: " << cols_ << "\n";
        return false;
    }

    /* skip over 'x' */
    if ((int)'x' != in.get() || in.fail()) {
        std::cerr << "expected 'x' in NxM not found!\n";
        return false;
    }

    /* get rows */
    in >> rows_;
    if (!in) {
        std::cerr << "could not read rows!\n";
        return false;
    }
    if (rows_ <= 0) {
        std::cerr << "invalid rows: " << rows_ << "\n";
        return false;
    }
    
    /* skip over ' ' */
    if ((int)' ' != in.get() || in.fail()) {
        std::cerr << "expected ' ' after NxM not found!\n";
        return false;
    }

    /* get bacons */
    in >> bacons_;
    if (!in) {
        std::cerr << "could not read bacons!\n";
        return false;
    }
    if (bacons_ <= 0) {
        std::cerr << "invalid bacons: " << bacons_ << "\n";
        return false;
    }

    /* skip over '\n' */
    if ((int)'\n' != in.get() || in.fail()) {
        std::cerr << "expected \\n on line 0 not found!\n";
        return false;
    }

    for (int r = 0; r < rows_; ++r) {
        /* read a row */
        for (int c = 0; c < cols_; ++c) {
            char ch;
            /* get ch */
            if (in.get(ch).fail()) {
                std::cerr << "error reading at (" << r << "," << c << ")\n";
                return false;
            }
            if ('P' == ch) {
                row::row_pred pred(r);
                /* XXX this is slow, expensive, dumb search */
                row_list_type::iterator el = std::find_if(city_rows_.begin(), city_rows_.end(), pred);
                if (el != city_rows_.end()) {
                    /* row already in city_rows_ */
                    el->city_cols.push_back(c);
                } else {
                    /* row not in city_rows_ */
                    row g(r, c);
                    city_rows_.push_back(g);
                }
            } else if ('.' != ch) {
                std::cerr << "invalid element: '" << ch << "'\n";
                return false;
            }
        }
        if ((int)'\n' != in.get() || in.fail()) {
            std::cerr << "expected \\n on row " << r << " not found!\n";
            return false;
        }
    }
    char ch;
    while (!in.get(ch).fail()) {
        if ('\n' != ch) {
            std::cerr << "unexpected characters at end!\n";
            return false;
        }
    }
    if (in.fail() && !in.eof()) {
        std::cerr << "error reading at end!\n";
        return false;
    }
    return true;
}

#endif // BACONGRAPH_HPP_
