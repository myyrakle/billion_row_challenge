using System.Diagnostics;
using System.Text;

const string OUTPUT_PATH = "outputs.txt";
const string MEASUREMENTS_PATH = "measurements.txt";

var expectOutput = File.ReadAllText(OUTPUT_PATH);

var stopwatch = Stopwatch.StartNew();

var got = Solution();

stopwatch.Stop();

Console.WriteLine($"Elapsed time: {stopwatch.ElapsedMilliseconds} ms");

if (got == expectOutput)
{
    Console.WriteLine("Passed");
}
else
{
    Console.WriteLine("Failed");
    Console.WriteLine($"Expected:\n{expectOutput}");
    Console.WriteLine($"Got:\n{got}");
}

string Solution()
{
    var hashmap = new Dictionary<string, Status>();

    using (var reader = new StreamReader(MEASUREMENTS_PATH))
    {
        string? line;
        while ((line = reader.ReadLine()) != null)
        {
            string[] parts = line.Split(';');
            string cityName = parts[0];
            int measurement = int.Parse(parts[1]);

            if (!hashmap.ContainsKey(cityName))
            {
                hashmap[cityName] = new Status
                {
                    min = measurement,
                    max = measurement,
                    total = measurement,
                    count = 1
                };
            }
            else
            {
                hashmap[cityName].min = Math.Min(hashmap[cityName].min, measurement);
                hashmap[cityName].max = Math.Max(hashmap[cityName].max, measurement);
                hashmap[cityName].total += measurement;
                hashmap[cityName].count++;
            }
        }
    }

    // Get Key List, then sort it
    var keyList = hashmap.Keys.ToList();
    keyList.Sort((a, b) => string.Compare(a, b, StringComparison.Ordinal));

    var result = new StringBuilder();

    foreach (var key in keyList)
    {
        var status = hashmap[key];
        var avg = status.total / status.count;
        result.Append($"{key}={status.min};{status.max};{avg}({status.total}/{status.count})\n");
    }

    return result.ToString();
}