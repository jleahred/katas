#include <future>
#include <iostream>
#include <string>

#include "zmq.hpp"
//#include "zmq_addon.hpp"

void PublisherThread(zmq::context_t* ctx)
{
    //  Prepare publisher
    std::cout << ".1" << std::flush;
    zmq::socket_t publisher(*ctx, zmq::socket_type::pub);
    std::cout << ".2" << std::flush;
    publisher.bind("tcp://127.0.0.1:5555");
    std::cout << ".3" << std::flush;

    // Give the subscribers a chance to connect, so they don't lose any messages
    std::this_thread::sleep_for(std::chrono::milliseconds(20));

    while (true) {
        zmq::message_t msg;

        //  Write three messages, each with an envelope and content
        //        publisher.send(zmq::str_buffer("A"));
        publisher.send(zmq::str_buffer("A/B/C/Message in A envelope"));
        //        publisher.send(zmq::str_buffer("B"));
        //        publisher.send(zmq::str_buffer("Message in B envelope"));
        //        publisher.send(zmq::str_buffer("C"));
        //        publisher.send(zmq::str_buffer("Message in C envelope"));
        std::this_thread::sleep_for(std::chrono::milliseconds(1000));
        std::cout << "." << std::flush;
    }
}

void PublisherThread2(zmq::context_t* ctx)
{
    //  Prepare publisher
    std::cout << ".a1" << std::flush;
    zmq::socket_t publisher(*ctx, zmq::socket_type::pub);
    std::cout << ".a1" << std::flush;
    publisher.connect("tcp://127.0.0.1:5555");
    std::cout << ".a1" << std::flush;

    // Give the subscribers a chance to connect, so they don't lose any messages
    std::this_thread::sleep_for(std::chrono::milliseconds(20));

    while (true) {
        zmq::message_t msg;

        //  Write three messages, each with an envelope and content
        //        publisher.send(zmq::str_buffer("A"));
        publisher.send(zmq::str_buffer("A/B/C/2222222222222222"));
        //        publisher.send(zmq::str_buffer("B"));
        //        publisher.send(zmq::str_buffer("Message in B envelope"));
        //        publisher.send(zmq::str_buffer("C"));
        //        publisher.send(zmq::str_buffer("Message in C envelope"));
        std::this_thread::sleep_for(std::chrono::milliseconds(1000));
    }
}

void PublisherThreadSniffer(zmq::context_t* ctx)
{
    //  Prepare publisher
    zmq::socket_t publisher(*ctx, zmq::socket_type::xpub);
    publisher.connect("tcp://127.0.0.1:5555");

    // Give the subscribers a chance to connect, so they don't lose any messages
    std::this_thread::sleep_for(std::chrono::milliseconds(20));

    while (true) {
        zmq::message_t msg;
        publisher.recv(msg);
        std::cout << "Thread 0: " << msg.to_string() << std::endl;

        //  Write three messages, each with an envelope and content
        //        publisher.send(zmq::str_buffer("A"));
        //        publisher.send(zmq::str_buffer("A/B/C/Message in A envelope"));
        //        publisher.send(zmq::str_buffer("B"));
        //        publisher.send(zmq::str_buffer("Message in B envelope"));
        //        publisher.send(zmq::str_buffer("C"));
        //        publisher.send(zmq::str_buffer("Message in C envelope"));
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
}

void SubscriberThread1(zmq::context_t* ctx)
{
    //  Prepare subscriber
    zmq::socket_t subscriber(*ctx, zmq::socket_type::sub);
    subscriber.connect("tcp://127.0.0.1:5555");

    //  Thread2 opens "A" and "B" envelopes
    subscriber.set(zmq::sockopt::subscribe, "");
    subscriber.set(zmq::sockopt::rcvtimeo, 100);
    //    subscriber.set(zmq::sockopt::subscribe, "M");

    while (1) {
        std::cout << "_" << std::flush;
        zmq::message_t msg;
        const auto rec = subscriber.recv(msg);
        if (rec) {
            std::cout << "Thread 2: " << msg.to_string() << std::endl;
        }
        //        auto pi = zmq::pollitem_t {};
        //        pi.socket = subscriber;
        //        pi.events = zmq::e
        //        std::vector<zmq::pollitem_t> pits = { pi };
        //        zmq::poll(pits, std::chrono::milliseconds(10));
        //        std::cout << "." << std::flush;
        //        zmq::message_t msg;
        //        if (pi.revents) {
        //            subscriber.recv(msg);
        //            std::cout << "Thread 2: " << msg.to_string() << std::endl;
        //        }
        //        // Receive all parts of the message
        //        std::vector<zmq::message_t> recv_msgs;
        //        zmq::recv_result_t result = zmq::recv_multipart(subscriber, std::back_inserter(recv_msgs));
        //        assert(result && "recv failed");
        //        assert(*result == 2);

        //        std::cout << "Thread2: [" << recv_msgs[0].to_string() << "] "
        //                  << recv_msgs[1].to_string() << std::endl;
    }
}

void SubscriberThread2(zmq::context_t* ctx)
{
    return;
    //  Prepare subscriber
    zmq::socket_t subscriber(*ctx, zmq::socket_type::sub);
    subscriber.connect("tcp://127.0.0.1:5555");

    //  Thread2 opens "A" and "B" envelopes
    subscriber.set(zmq::sockopt::subscribe, "A/B/C/D");
    //    subscriber.set(zmq::sockopt::subscribe, "M");

    while (1) {
        zmq::message_t msg;
        subscriber.recv(msg);
        std::cout << "Thread 2: " << msg.to_string() << std::endl;
        //        // Receive all parts of the message
        //        std::vector<zmq::message_t> recv_msgs;
        //        zmq::recv_result_t result = zmq::recv_multipart(subscriber, std::back_inserter(recv_msgs));
        //        assert(result && "recv failed");
        //        assert(*result == 2);

        //        std::cout << "Thread2: [" << recv_msgs[0].to_string() << "] "
        //                  << recv_msgs[1].to_string() << std::endl;
    }
}

// void SubscriberThread2(zmq::context_t* ctx)
//{
//     //  Prepare our context and subscriber
//     zmq::socket_t subscriber(*ctx, zmq::socket_type::sub);
//     subscriber.connect("tcp://localhost:5555");

//    //  Thread3 opens ALL envelopesa
//    //    subscriber.set(zmq::sockopt::subscribe, "A");
//    //    subscriber.send(zmq::str_buffer("\0x01A/"));
//    subscriber.send(zmq::str_buffer("A/"));

//    while (1) {
//        zmq::message_t msg;
//        subscriber.recv(msg);
//        std::cout << "Thread 3: " << msg.to_string() << std::endl;
//        //        // Receive all parts of the message
//        //        std::vector<zmq::message_t> recv_msgs;
//        //        zmq::recv_result_t result = zmq::recv_multipart(subscriber, std::back_inserter(recv_msgs));
//        //        assert(result && "recv failed");
//        //        assert(*result == 2);

//        //        std::cout << "Thread3: [" << recv_msgs[0].to_string() << "] "
//        //                  << recv_msgs[1].to_string() << std::endl;
//    }
//}

int main()
{
    /*
     * No I/O threads are involved in passing messages using the inproc transport.
     * Therefore, if you are using a ØMQ context for in-process messaging only you
     * can initialise the context with zero I/O threads.
     *
     * Source: http://api.zeromq.org/4-3:zmq-inproc
     */
    zmq::context_t ctx(8);
    auto thread0 = std::async(std::launch::async, PublisherThreadSniffer, &ctx);
    auto thread1 = std::async(std::launch::async, PublisherThread, &ctx);
    auto thread10 = std::async(std::launch::async, PublisherThread2, &ctx);
    std::this_thread::sleep_for(std::chrono::milliseconds(3000));

    // Give the publisher a chance to bind, since inproc requires it
    std::this_thread::sleep_for(std::chrono::milliseconds(10));

    auto thread2 = std::async(std::launch::async, SubscriberThread1, &ctx);
    auto thread3 = std::async(std::launch::async, SubscriberThread2, &ctx);

    //    thread0.wait();
    //    thread1.wait();
    //    thread2.wait();
    //    thread3.wait();

    /*
     * Output:
     *   An infinite loop of a mix of:
     *     Thread2: [A] Message in A envelope
     *     Thread2: [B] Message in B envelope
     *     Thread3: [A] Message in A envelope
     *     Thread3: [B] Message in B envelope
     *     Thread3: [C] Message in C envelope
     */
}
