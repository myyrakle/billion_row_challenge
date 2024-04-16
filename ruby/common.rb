require 'time'

OUTPUT_PATH = "outputs.txt"
MEASUREMENTS_PATH = "measurements.txt"

class Timer
    def initialize
        @start = Time.now
    end

    def elapsed_as_milliseconds
        (Time.now - @start) * 1000
    end
end