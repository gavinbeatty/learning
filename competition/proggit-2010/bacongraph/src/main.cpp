/* http://proggitquiz.com/challenge/4/ */

#include <bacongraph.hpp>
#include <solution.hpp>
#include <taxicab.hpp>

#include <fstream>

int main(int argc, char* argv[])
{
    if (1 == argc) {
        std::cerr << "usage: bacongraph <file1>...\n";
        return 1;
    }
    for (int file_index = 1; file_index < argc; ++file_index) {
        std::ifstream input(argv[file_index]);
        bacongraph graph;
        input >> graph;
        if (input.fail()) {
            std::cerr << "failed on input " << (file_index-1) << ": " << argv[file_index] << "\n";
            return 1;
        }
        // XXX for basic testing
        std::cout << graph;

        solution soln;
        if (!soln.init(graph)) {
            std::cerr << "error solving graph\n";
            return 1;
        }
        std::cout << soln;
    }
    return 0;
}

