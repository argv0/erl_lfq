erl_lfq
=======

[![Build Status](https://secure.travis-ci.org/[YOUR_GITHUB_USERNAME]/[YOUR_PROJECT_NAME].png)]

erl_lfq is a lock-free, single-producer, single-consumer FIFO queue for Erlang binaries,
implemented in C++ and wrapped in a NIF.  

Building
--------

Information on building and installing [Erlang/OTP](http://www.erlang.org)
can be found [here](https://github.com/erlang/otp/wiki/Installation)
([more info](https://github.com/erlang/otp/blob/master/INSTALL.md)).

#### Building erl_lfq

erl_lfq uses gcc-specific atomic builtins, so building requires a relatively recent gcc/g++.

```sh
$ git clone git://github.com/argv0/erl_lfq.git
$ cd erl_lfq
$ make
$ make test
```

API 
---

`erl_lfq:new() -> {ok, QRef}` : create a new queue

`erl_lfq:in(QRef, binary()) -> ok` : add an item to the queue

`erl_lfq:out(QRef) -> binary() | empty` : remove the next item from the queue

`erl_lfq:byte_size(QRef) -> non_neg_integer()` : total size in bytes of items in the queue

`erl_lfq:len(QRef) -> non_neg_integer()` : number of items in the queue


Contributing to erl_lfq
=======================

Pull requests and branching
---------------------------

Use one topic branch per pull request.

Do not commit to master in your fork.

Provide a clean branch without any merge commits from upstream.

Usually you should squash any intermediate commits into the original single commit.

Code style
----------

Do not introduce trailing whitespace.

Do not mix spaces and tabs.

Do not introduce lines longer than 80 characters.

[erlang-mode (emacs)](http://www.erlang.org/doc/man/erlang.el.html) indentation is preferred.
vi-only users are encouraged to give [Vim emulation](http://emacswiki.org/emacs/Evil)
([more info](https://gitorious.org/evil/pages/Home)) a try.

Writing Commit Messages
-----------------------

Structure your commit message like this:

<pre>
One line summary (less than 50 characters)

Longer description (wrap at 72 characters)
</pre>

### Summary

* Less than 50 characters
* What was changed
* Imperative present tense (fix, add, change)
  * `Fix bug 123`
  * `Add 'foobar' command`
  * `Change default timeout to 123`
* No period

### Description

* Wrap at 72 characters
* Why, explain intention and implementation approach
* Present tense

### Atomicity

* Break up logical changes
* Make whitespace changes separately
