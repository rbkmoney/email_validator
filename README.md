Email validator for Erlang
==========================

[![Build Status](https://github.com/rbkmoney/email_validator/workflows/CI/badge.svg)](https://github.com/rbkmoney/email_validator/actions?query=branch%3Amaster+workflow%3A"CI") [![Erlang Versions](https://img.shields.io/badge/Supported%20Erlang%2FOTP-21.0%20to%2023.0-blue)](http://www.erlang.org)

Library for email address validation based on [RFC5321 (Simple Mail Transfer Protocol)](https://tools.ietf.org/rfc/rfc5321.txt)
with additions from [RFC6532 (Internationalized Email Headers)](https://tools.ietf.org/rfc/rfc6532.txt), [RFC6531 (SMTP Extension for Internationalized Email)](https://tools.ietf.org/rfc/rfc6531.txt)
and [RFC3986 (Uniform Resource Identifier (URI): Generic Syntax)](https://tools.ietf.org/html/rfc3986#appendix-A) for stricter IP address grammar.

Using the library
-----------------
Add library as dependency in `rebar.config`

    {deps, [
        {email_validator, "1.0.0"}
        ...
    ]}.

Add `email_validator` as application dependency

    {application, app,
         [
          {applications, [
                          ...
                          email_validator
                         ]},
          ...
         ]}. 

Use `email_validator:validate/1` for email validation.
