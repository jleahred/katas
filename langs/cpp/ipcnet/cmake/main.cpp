#include <chrono>
#include <iostream>
#include <thread>

#include "../src/ipc.h"

int main() {
    try {
        // Queue and cursor in current working directory.
        chi::QueueWriter mut_writer("example.queue");
        mut_writer.write("uno");
        mut_writer.write("dos");
        mut_writer.write("tres");

        // Another process/reader would read from the same file.
        chi::QueueReader mut_reader("example.queue");
        {
            const auto result = mut_reader.read(2);  // read up to 2 records
            if (result.signature_changed) {
                std::cout << "Queue was reset; starting over\n";
            }
            if (result.inconsistent) {
                std::cout << "Corrupt data\n";
                return 1;
            }
            for (const auto& msg : result.messages) {
                std::cout << "Read: " << msg << "\n";
            }
        }

        // Write while the reader could still be reading.
        mut_writer.write("cuatro");
        mut_writer.write("cinco");

        std::this_thread::sleep_for(std::chrono::milliseconds(100));  // simulate work time

        {
            const auto result = mut_reader.read(10);  // read the remaining
            for (const auto& msg : result.messages) {
                std::cout << "Read: " << msg << "\n";
            }
        }

        // Reset the queue (new signature; cursor will adjust on next read).
        mut_writer.reset();
        mut_writer.write("despues del reset");

        const auto result = mut_reader.read(5);
        if (result.signature_changed) {
            std::cout << "New signature detected after reset\n";
        }
        for (const auto& msg : result.messages) {
            std::cout << "Read: " << msg << "\n";
        }

    } catch (const std::exception& ex) {
        std::cerr << "Error: " << ex.what() << "\n";
        return 1;
    }
    return 0;
}
