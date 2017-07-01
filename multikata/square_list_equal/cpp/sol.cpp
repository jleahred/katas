#include <vector>
#include <algorithm>

template<typename T>
bool comp(T& a, T& b)
{
    auto sort = [] (auto&& c) { std::sort(c.begin(), c.end()); return c; };
    auto square = [] (auto&& c) { for(auto& e:c) e = e*e; return c; };

    return sort(square(a))==sort(b);
}
