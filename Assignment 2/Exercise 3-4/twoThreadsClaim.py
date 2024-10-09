import statistics
import threading
import time

def bench(n_threads,seq_iter,iter):
    def wrapper(func):
        def inner(*args, **kwargs):
            exec_time = []

            def run_func():
                for i in range(seq_iter):
                    func(*args, **kwargs)

            for i in range (iter):
                start_time = time.perf_counter()
                jobs = []

                # Threads creation
                for j in range(n_threads):
                    jobs.append(threading.Thread(target=run_func))
                
                # Start threads
                for j in range(n_threads):
                    jobs[j].start()

                # Wait for all threads to join
                for j in range(n_threads):
                    jobs[j].join()
                
                # Compute statistics
                end_time = time.perf_counter()
                exec_time.append(end_time - start_time)
                mean_time = statistics.mean(exec_time)
            
                if len(exec_time) > 1:
                    variance_time = statistics.variance(exec_time)
                else:
                    variance_time = 0

                return {
                    'fun': func.__name__,
                    'args': args,
                    'n_threads': n_threads,
                    'seq_iter': seq_iter,
                    'iter': iter,
                    'mean': mean_time,
                    'variance': variance_time
                }
        return inner
    return wrapper

# Function that executes a pause, simulating a low CPU load (I/O-bound)
def just_wait(n):
    time.sleep(n * 0.1)

# Function that executes a heavy loop, simulating a high CPU load (CPU-bound)
def grezzo(n):
    for _ in range(2**n):
        pass

# Test function to run bench decorator on different configurations
def test(iter, fun, args):
    results = []

    # Executes fun 16 times on a single thread
    result_1_thread = bench(n_threads=1, seq_iter=16, iter=iter)(fun)(*args)
    results.append(result_1_thread)

    # Executes fun 8 times on 2 threads
    result_2_threads = bench(n_threads=2, seq_iter=8, iter=iter)(fun)(*args)
    results.append(result_2_threads)

    # Executes fun 4 times on 4 threads
    result_4_threads = bench(n_threads=4, seq_iter=4, iter=iter)(fun)(*args)
    results.append(result_4_threads)

    # Executes fun 2 times on 8 threads
    result_8_threads = bench(n_threads=8, seq_iter=2, iter=iter)(fun)(*args)
    results.append(result_8_threads)

    # Write results to output file
    outputFile = open("./output.txt", "a")
    for result in results:
        outputFile.write(str(result) + "\n")
    outputFile.close()

# Example of running the test function with different workloads
if __name__ == "__main__":
    # Test on just_wait (I/O Bound) function with iter=3
    test(iter=3, fun=just_wait, args=(10,))

    # Test on grezzo (CPU Bound) function with iter=3
    test(iter=3, fun=grezzo, args=(10,))

# Results of experimentation:
# As expected, we can see from the results that the execution of the I/O-bound function (just_wait) gains the advantage of using multiple threads.
# On the other hand, the execution of the CPU-bound function (grezzo) is slower when using multiple threads, and we do not gain any advantage from using threads.
# This is due to the Python GIL (Global Interpreter Lock) that prevents multiple threads from executing simultaneously.
# In particular, I/O-bound functions are able to release the GIL and allow other threads to be scheduled.
# CPU-bound functions, however, cannot release the GIL, so the threads are not able to be scheduled efficiently simultaneously.
# Another problem with CPU-bound functions is the signaling overhead because every 100 ticks, the Python GIL locks a mutex and signals
# on a condition variable or semaphore where other threads are waiting. This triggers many system calls that lead to significant performance degradation.