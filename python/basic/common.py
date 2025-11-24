import time

OUTPUT_PATH = "outputs.txt"
MEASUREMENTS_PATH = "measurements.txt"

class Timer:
    def __init__(self):
        self.start = time.time()
        
    def elapsed_as_milliseconds(self):
        return (time.time() - self.start) * 1000