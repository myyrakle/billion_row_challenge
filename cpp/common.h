#include <string>
#include <chrono>

constexpr const char *OUTPUT_PATH = "outputs.txt";
constexpr const char *MEASUREMENTS_PATH = "measurements.txt";

//시간 측정용 클래스입니다.
class Timer
{
public:
    void start() //타이머를 가동합니다.
    {
        start_point = std::chrono::system_clock::now();
    }
    void stop() //타이머를 중단합니다.
    {
        stop_point = std::chrono::system_clock::now();
    }
    const Timer& stop_and() //타이머를 중단하고 자기 자신을 반환합니다. 바로 get 메서드들을 쓰면 됩니다.
    {
        stop();
        return *this;
    }
    void clear() //값을 초기화합니다.
    {
        start_point = std::chrono::time_point<std::chrono::system_clock>::min();
        stop_point = std::chrono::time_point<std::chrono::system_clock>::min();
    }
public:
    template<class seconds_t>
    long long get_time() const //측정 메서드의 템플릿입니다.
    {
        return std::chrono::duration_cast<seconds_t>(stop_point - start_point).count();
    }
    long long get_milli() const //밀리세컨드 단위입니다.
    {
        return get_time<std::chrono::milliseconds>();
    }
    long long get_seconds() const //세컨드 단위입니다.
    {
        return get_time<std::chrono::seconds>();
    }
private:
    std::chrono::time_point<std::chrono::system_clock> start_point; //시작지점입니다.
    std::chrono::time_point<std::chrono::system_clock> stop_point; //중단지점입니다.
public:
    Timer() = default;
    Timer(const Timer&) = default;
    Timer(Timer&&) = default;
    Timer& operator= (const Timer&) = default;
    Timer& operator= (Timer&&) = default;
    virtual ~Timer() = default;
};