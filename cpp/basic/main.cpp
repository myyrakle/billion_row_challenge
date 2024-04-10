#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdint>
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <utility>
#include "../common.h"

struct Status
{
    std::int64_t min;
    std::int64_t max;
    std::int64_t count;
    std::int64_t total;
};

auto split(const std::string &text, char seperator) -> std::vector<std::string>
{
    std::vector<std::string> result;

    auto index = text.find(seperator);
    for (int i = 0; index < text.size(); i++)
    {
        if (text[i] == seperator)
        {
            result.push_back(text.substr(0, i));
            result.push_back(text.substr(i + 1, text.size()));

            break;
        }
    }

    return result;
}

auto solution() -> std::string
{
    std::ifstream file(MEASUREMENTS_PATH);
    if (!file)
    {
        std::cerr << "File could not be opened\n";
        throw std::runtime_error("File could not be opened");
    }

    std::unordered_map<std::string, Status> status_map;
    std::string line;
    while (std::getline(file, line))
    {
        auto parts = split(line, ';');

        auto city_name = std::move(parts[0]);
        auto measurement = std::stoll(parts[1]);

        if (status_map.count(city_name) == 0)
        {
            status_map[city_name] = Status{
                .min = measurement,
                .max = measurement,
                .count = 1,
                .total = measurement};
        }
        else
        {
            const auto &old_status = status_map[city_name];

            status_map[city_name] = Status{
                .min = std::min(old_status.min, int64_t(measurement)),
                .max = std::max(old_status.max, int64_t(measurement)),
                .count = old_status.count + 1,
                .total = old_status.total + measurement};
        }
    }

    std::string result;

    std::vector<std::string> city_names;
    for (const auto &[city_name, status] : status_map)
    {
        city_names.push_back(city_name);
    }
    std::sort(city_names.begin(), city_names.end());

    for (const auto &city_name : city_names)
    {
        auto status = status_map[city_name];
        const auto avg = status.total / status.count;
        auto sstream = std::stringstream();
        sstream << city_name << "=" << status.min << ";" << status.max << ";" << avg << "(" << status.total << "/" << status.count << ")" << std::endl;

        result += sstream.str();
    }

    return result;
}

int main()
{
    std::ifstream file(OUTPUT_PATH);
    std::stringstream buffer;

    buffer << file.rdbuf();

    const auto expect_outputs = buffer.str();

    auto timer = Timer();
    timer.start();
    const auto got = solution();
    timer.stop();
    const auto elaped = timer.get_milli();

    std::cout << "Elapsed: " << elaped << "ms" << std::endl;

    if (got == expect_outputs)
    {
        std::cout << "Test passed!" << std::endl;
    }
    else
    {
        std::cout << "Test failed!" << std::endl;
        std::cout << "Expect: " << expect_outputs << std::endl;
        std::cout << "Got: " << got << std::endl;
    }

    return 0;
}
