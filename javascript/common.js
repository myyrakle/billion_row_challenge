const OUTPUT_PATH = 'outputs.txt';
const MEASUREMENTS_PATH = "measurements.txt"

class Timer {
    constructor() {
        this.start = new Date()
    }

    elapsedAsMilliSeconds() {
        return new Date() - this.start;
    }
}

module.exports = {
    OUTPUT_PATH,
    MEASUREMENTS_PATH,
    Timer
}
