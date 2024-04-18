require_relative './common.rb'

def solution
    map = {}

    File.foreach(MEASUREMENTS_PATH) do |line|
        parts = line.split(";")
        city_name = parts[0]
        measurement = parts[1].to_i

        if map[city_name]
            map[city_name] = {
                min: [map[city_name][:min], measurement].min,
                max: [map[city_name][:max], measurement].max,
                total: map[city_name][:total] + measurement,
                count: map[city_name][:count] + 1,
            }
        else
            map[city_name] = {
                min: measurement,
                max: measurement,
                total: measurement,
                count: 1,
            }
        end
    end

    sorted_map = map.sort.to_h
    bucket = ""
    sorted_map.each do |city_name, status|
        avg = status[:total] / status[:count]
        line = "#{city_name}=#{status[:min]};#{status[:max]};#{avg}(#{status[:total]}/#{status[:count]})\n"
        bucket += line
    end

    bucket
end

def main 
    timer = Timer.new

    expect_output = File.read(OUTPUT_PATH)
    
    got = solution

    if got == expect_output
        puts "Test Passed"
    elsif 
        puts "Test Failed\nExpected: #{expect_output}\nGot: #{got}"
    end
    
    elapsed = timer.elapsed_as_milliseconds
    
    puts "Elapsed: #{elapsed} ms"
end

main