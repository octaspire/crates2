#!/bin/sh
# Octaspire Crates 2 - Puzzle Game
# Copyright 2020, 2021 octaspire.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
OS=$(uname)

if [ "$OS" = "OpenBSD" ]
then
    echo "===================================================================================="
    echo "Running in OpenBSD. This script requires the following line in your /etc/doas.conf:"
    echo ""
    echo "permit nopass ${USER} as root cmd /bin/cp args crates2 /usr/local/bin/"
    echo ""
    echo "If this script doesn't work, please add the above line to your /etc/doas.conf."
    echo "Or copy the executable to /usr/local/bin manually."
    echo "===================================================================================="
    doas /bin/cp crates2 /usr/local/bin/
else
    cp crates2 /usr/local/bin/
fi
