package ru.maxkar

/**
 * This package provides support for simple tasks components (hunks).
 * <h1>Summary</h1>
 * Each hunk is some kind of an execution abstraction. It may contain
 * calculated value or only a process which yield value later. Unlike
 * monads and monad transformers, hunks are mutable by default, supports
 * addition of listeners and may evaluate result "asynchronously".
 * <p>Using hunks you may write "data processing flows" which does
 * not depend on a concrete execution model for it's dependencies.
 * Consider writing a simple web-server. One of the tasks is to receive
 * user request. You may write your own bunch of hunks:
 * <pre>
 * def getConnection() : Hunk[UserConnection]
 * def readRequest(conn : UserConnection) : Hunk[UserRequest]
 * //Define a fetch for the user request:
 * def getRequestHunk() : Hunk[UserRequest] =
 *   readRequest _ &lt;**&gt; getConnection()
 * </pre>
 * As you can see, concrete implementation of this processing is not
 * relevant for the user. Underlying hunks may use IO threads (two threads
 * for each client), they may use selectors and process request in the
 * separate threads, etc... At this point you may not care about
 * "execution model" details such as "in which thread this will be
 * executed?", "should I wait for the completion?", "should I receive a
 * raw data or it's execution status?". You may focus on the dependencies of
 * job parts only.
 * <h1>Threading</h1>
 * All instances of hunks must be thread-safe. It must be save to call
 * any hunk method from any thread.
 * <p>Threads, on which callbacks are invoked, are unspecified. So,
 * callbacks may be invoked on the thread, which performed the task,
 * on some dispatch thread, on a calling thread (callback may be called
 * at the time of the onComplete/onResult call) or somewhere other. So
 * if your callback requires some strict discipline, you should ensure that
 * discipline yourself.
 * <p>Completion callbacks may be called during the &quot;on...&quot; method call.
 * For example, <code>hunk.onResult(cb)</code> may call <code>cb</code> before
 * returning from the onResult call! Callback users must be aware of
 * this behavior.
 * <h1>Side effects/traces</h1>
 * Along with some result, each hunk may contain a list of additional
 * information. For example, it may contain a list of messages for the
 * given task. Such bits of informations are called &quot;tracec&quot;.
 * When you combine/aggregate tasks, all traces are combined in such a
 * manner that there is only one instance of the trace message from one
 * underlying task. Consider tasks A and B with no dependencies, tasks
 * C and D depends on both A and B and task E depends on C, D and A.
 * Task a procudes traces T1 and T2, task D produces another instance of
 * T1. When you get all traces from E, you will get (T1, T2, T1). One
 * instance of T1 if from task A, one from task D. But there is only
 * one instance of T1 produced by A despite the fact that task A occurs
 * three times in full dependency tree for the task A.
 * <p>Traces are a good way to provide consistent information when
 * you run several simulateous processings on the items of same kind. For
 * example, compiler may process several input files simulateously. And
 * each file may produce bunch of several warning messages. You may use
 * some kind of a "logger", but you still should use some way to synchronize
 * access to an output (to prevent a complete mixup of messages). And it
 * will be a nice feature to write all messages for the same file one-by-one
 * (not intermixed with messages for another files). In this case trace is
 * exactly what you need. For the topmost "do-all-actions-for-the-file" task
 * you will get all the subtask traces for that task.
 * <p> Users are encouraged to provide all necessary information as
 * task traces and not "direct side effects". Note, that each such
 * "side-effect" may be described as SideEffectTrace(effect : () ⇒  Unit).
 * At the top-level you may collect all that side-effects and run them
 * in some particular order.
 * <h2>Mapping</h2>
 * Be aware, that mapping either exceptions or traces creates a new
 * node, which is unrelated with the previous task. In the following
 * code a and b are considered completely different tasks and will provide
 * two sets of base's mapped side-effects.
 * <pre>
 * base : Hunk[R, F, T1] = ...
 * a = base.mapSide(mapper)
 * b = base.mapSide(mapper)
 * c = fn &lt;*&gt; a &lt;*&gt; b
 * </pre>
 * <h1>Exceptions policy</h1>
 * <h2>Exceptions in tasks</h2>
 * Throwing an exception from the hunk body (hunk implementation) results
 * in a "Failure-exception" result for that hunk.
 * <h2>Exceptions in combinators</h2>
 * Each task may be terminated by explicitly returing a failure of throwing
 * an exception. When tasks combinators receive in exception in some of the
 * child tasks, it usually selects only a first exception. For example,
 * if you have <code>f &lt;*&gt; g</code> and got two exceptions (both for
 * f and g), then you will get only a f's exception as a cause of the failure.
 * <h2>Exceptions in callbacks</h2>
 * Users should not throw exceptions in hunk callbacks (on*). Any such
 * exceptions will be ignored. Hunk writers must ensure that any exception from
 * the callbacks does not affect other callbacks.
 */
package object hunk {
}
