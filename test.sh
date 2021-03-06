#/usr/bin/env bash

# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

bin/uncaught-exception-demo id 2>&1 >/dev/null | grep MyException
bin/uncaught-exception-demo displayUncaughtException 2>&1 > /dev/null | grep displayException
bin/uncaught-exception-demo withDisplayExceptionHandler 2>&1 > /dev/null | grep displayException
bin/uncaught-exception-demo setDisplayExceptionHandler 2>&1 > /dev/null | grep displayException
