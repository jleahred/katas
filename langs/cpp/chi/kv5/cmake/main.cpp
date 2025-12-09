#include "../src/kv5.h"
#include "../chi/ipcnet/src/netipc.h"

#include <filesystem>
#include <iostream>

int main() {
    using namespace chi;

    const auto base_dir = std::filesystem::path{"demo_data"};
    std::filesystem::create_directories(base_dir);
    const auto queue_root = base_dir / "queues";
    std::filesystem::create_directories(queue_root);

    KvStoreConfig config_a;
    config_a.db_path = base_dir / "a.db";
    config_a.queue_root = queue_root;

    KvStoreConfig config_b;
    config_b.db_path = base_dir / "b.db";
    config_b.queue_root = queue_root;

    KvStore store_a(config_a);
    KvStore store_b(config_b);

    const QueuePathResolver resolver = [queue_root](const std::string& queue_name) {
        return queue_root / (queue_name + ".queue");
    };
    chi::ServerWriter mut_server_writer(config_a.replica_host, config_a.replica_writer_port, resolver);
    chi::ServerReader mut_server_reader(config_a.replica_host, config_a.replica_reader_port, resolver);
    mut_server_writer.start();
    mut_server_reader.start();

    const std::string queue_name{"replica_ab"};

    // Prepare replication: A -> queue, B <- queue.
    store_a.replicate_to(queue_name);
    store_b.replicate_from(queue_name);

    // Write on A.
    store_a.set_value("/root/demo/item1", R"({"payload":{"id":1,"name":"alphaa", "range" : [1, 2, 3]}})");
    store_a.set_value("/root/demo/item2", R"({"payload":{"id":2,"name":"beta"}})");
    store_a.set_value("/root/demo2/item21", R"({"payload":{"id":1,"name":"alphaa"}})");
    store_a.set_value("/root/", R"({"payload":{"id":0,"name":"rooot"}})");

    // Send a batch to the queue (default scan_window).
    store_a.send2replica(queue_name, 0);

    // Read from queue into B's import table.
    store_b.get_from_replica(0);

    // Promote import table to master on B.
    store_b.replica2master();

    {
        // Verify the replicated value using a JSON selector.
        const auto values = store_b.get("/root/demo/(.*)/{.payload.name}", 0);
        for (const auto& entry : values.values) {
            std::cout << "[B] replicated value: key=" << entry.first << " value=" << entry.second << '\n';
        }
    }

    {
        const auto key = "/";
        std::cout << "print paths/values from " << key << std::endl;
        const auto found = store_b.get(key, 0);
        for (const auto& path : found.paths) {
            std::cout << "[B] replicated path: " << path << '\n';
        }
        for (const auto& entry : found.values) {
            std::cout << "[B] replicated values: key=" << entry.first << " value=" << entry.second << '\n';
        }
        std::cout << "---------------" << std::endl;
    }
    {
        const auto key = "/(.*)";
        std::cout << "print paths/values from " << key << std::endl;
        const auto found = store_b.get(key, 0);
        for (const auto& path : found.paths) {
            std::cout << "[B] replicated path: " << path << '\n';
        }
        for (const auto& entry : found.values) {
            std::cout << "[B] replicated values: key=" << entry.first << " value=" << entry.second << '\n';
        }
        std::cout << "---------------" << std::endl;
    }


    {
        const auto key = "/root/";
        std::cout << "print paths/values from " << key << std::endl;
        const auto found = store_b.get(key, 0);
        for (const auto& path : found.paths) {
            std::cout << "[B] replicated path: " << path << '\n';
        }
        for (const auto& entry : found.values) {
            std::cout << "[B] replicated values: key=" << entry.first << " value=" << entry.second << '\n';
        }
        std::cout << "---------------" << std::endl;
    }

    {
        const auto key = "/root/(.*)";
        std::cout << "print paths/values from " << key << std::endl;
        const auto found = store_b.get(key, 0);
        for (const auto& path : found.paths) {
            std::cout << "[B] replicated path: " << path << '\n';
        }
        for (const auto& entry : found.values) {
            std::cout << "[B] replicated values: key=" << entry.first << " value=" << entry.second << '\n';
        }
        std::cout << "---------------" << std::endl;
    }

    {
        const auto key = "/root/(.*)/(.*)";
        std::cout << "print paths/values from " << key << std::endl;
        const auto found = store_b.get(key, 0);
        for (const auto& path : found.paths) {
            std::cout << "[B] replicated path: " << path << '\n';
        }
        for (const auto& entry : found.values) {
            std::cout << "[B] replicated values: key=" << entry.first << " value=" << entry.second << '\n';
        }
        std::cout << "---------------" << std::endl;
    }

    {
        const auto key = "/root/demo/item1";
        std::cout << "print paths/values from " << key << std::endl;
        const auto found = store_b.get(key, 0);
        for (const auto& path : found.paths) {
            std::cout << "[B] replicated path: " << path << '\n';
        }
        for (const auto& entry : found.values) {
            std::cout << "[B] replicated values: key=" << entry.first << " value=" << entry.second << '\n';
        }
        std::cout << "---------------" << std::endl;
    }

    {
        const auto key = "/root/demo/(.*){.payload.id .payload.range}";
        std::cout << "print paths/values from " << key << std::endl;
        const auto found = store_b.get(key, 0);
        for (const auto& path : found.paths) {
            std::cout << "[B] replicated path: " << path << '\n';
        }
        for (const auto& entry : found.values) {
            std::cout << "[B] replicated values: key=" << entry.first << " value=" << entry.second << '\n';
        }
        std::cout << "---------------" << std::endl;
    }

    {
        const auto key = "/root/demo/(.*){.payload.id .payload.range[1] .payload .payload.range[1..6]}";
        std::cout << "print paths/values from " << key << std::endl;
        const auto found = store_b.get(key, 0);
        for (const auto& path : found.paths) {
            std::cout << "[B] replicated path: " << path << '\n';
        }
        for (const auto& entry : found.values) {
            std::cout << "[B] replicated values: key=" << entry.first << " value=" << entry.second << '\n';
        }
        std::cout << "---------------" << std::endl;
    }

    std::cout << "end" << std::endl;

    mut_server_writer.stop();
    mut_server_reader.stop();
    return 0;
}
