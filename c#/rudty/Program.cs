using System.Diagnostics;
using System.IO.MemoryMappedFiles;
using System.Text;

const string OUTPUT_PATH = "outputs.txt";
const string MEASUREMENTS_PATH = "measurements.txt";
const int DEFAULT_CAPACITY = 64;

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

static unsafe Dictionary<int, Status> RunInThread(byte* fileData, long pos, long end)
{
    // Dictionary key 는 string 대신 hashCode 를 사용합니다 
    var localHashMap = new Dictionary<int, Status>(DEFAULT_CAPACITY);
    var cityNameByteArray = new byte[DEFAULT_CAPACITY];
    while (pos < end)
    {
        var hashCode = 0;
        var measurement = 0;
        var cityNameWriteIndex = 0;

        do
        {
            var current = fileData[pos];
            cityNameByteArray[cityNameWriteIndex] = current;
            cityNameWriteIndex += 1;
            hashCode = 31 * hashCode + (current & 0xff);
            pos += 1L;
        } while (fileData[pos] != ';');

        pos += 1; // b[pos] == ';' 이므로 1 추가

        do
        {
            measurement *= 10;
            measurement += (fileData[pos] - '0');
            pos += 1L;
        } while (fileData[pos] != '\n');

        pos += 1; // b[pos] == '\n' 이므로 1 추가

        if (!localHashMap.TryGetValue(hashCode, out var status))
        {
            var cityName = Encoding.UTF8.GetString(cityNameByteArray, 0, cityNameWriteIndex);
            localHashMap[hashCode] = new Status
            {
                cityName = cityName,
                min = measurement,
                max = measurement,
                total = measurement,
                count = 1
            };
        }
        else
        {
            status.min = Math.Min(status.min, measurement);
            status.max = Math.Max(status.max, measurement);
            status.total += measurement;
            status.count += 1;
        }
    }

    return localHashMap;
}

static unsafe string Solution()
{
    /////////////////////////////////////////////////////////////////////////////////////////
    // 전역
    var fileInfo = new FileInfo(MEASUREMENTS_PATH);
    var fileSize =  fileInfo.Length;
    var processorCount = Environment.ProcessorCount;
    var threadSize = processorCount - 1;

    using var mmf = MemoryMappedFile.CreateFromFile(MEASUREMENTS_PATH, FileMode.Open, null, 0, MemoryMappedFileAccess.Read);
    using var accessor = mmf.CreateViewAccessor(0, 0, MemoryMappedFileAccess.Read);

    byte* fileData = null;  // <- 전체 파일 데이터 포인터 

    var accessorSafeMemoryMappedViewHandle = accessor.SafeMemoryMappedViewHandle;
    accessorSafeMemoryMappedViewHandle.AcquirePointer(ref fileData);
    /////////////////////////////////////////////////////////////////////////////////////////
    var hashMaps = new Dictionary<int, Status>[processorCount];
    var threads = new Thread[threadSize];
    var workSize = fileSize / processorCount;
    var offset = 0L;

    // 0부터 (N - 1) 까지 
    for (var i = 0; i < threadSize; ++i)
    {
        var currentIt = i;
        var currentOffset = offset;
        var currentEnd = offset + workSize;

        while (fileData[currentEnd] != '\n')
        {
            currentEnd += 1L;
        }

        offset = currentEnd + 1L;
        
        var thread = new Thread(() =>
        {
            hashMaps[currentIt] = RunInThread(fileData, currentOffset, currentEnd);
        });
        thread.UnsafeStart();
        threads[i] = thread;
    }

    // 마지막
    hashMaps[threadSize] = RunInThread(fileData, offset, fileSize);

    for (var i = 0; i < threadSize; ++i)
    {
        threads[i].Join();
    }

    accessorSafeMemoryMappedViewHandle.ReleasePointer();

    // Get Key List, then sort it
    var globalHashMap = new Dictionary<int, Status>(DEFAULT_CAPACITY);
    foreach (var hashMap in hashMaps)
    {
        foreach (var (hash, elem) in hashMap)
        {
            if (globalHashMap.TryGetValue(hash, out var status))
            {
                status.count += elem.count;
                status.total += elem.total;
                status.max = Math.Max(status.max, elem.max);
                status.min = Math.Min(status.min, elem.min);
            }
            else
            {
                globalHashMap[hash] = elem;
            }
        }
    }

    var valueList = globalHashMap.Values.ToList();
    valueList.Sort((lhs, rhs) => string.Compare(lhs.cityName, rhs.cityName, StringComparison.Ordinal));

    var result = new StringBuilder();

    foreach (var status in valueList)
    {
        var avg = status.total / status.count;
        result.Append($"{status.cityName}={status.min};{status.max};{avg}({status.total}/{status.count})\n");
    }

    return result.ToString();
}

public class Status
{
    public string cityName = null!;
    public long min;
    public long max;
    public long total;
    public long count;
};