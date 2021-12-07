#!/usr/bin/env bash

###############################################################################
# Entrypoint for the checker dev container
###############################################################################

set -e

# Changes to gid and uid require root access in the container
if [ "${EUID}" -ne 0 ]
  then echo "This script must be run as root."
  exit 1
fi

# If a speific UID is supplied, make sure that the container user has that UID.
# This helps reduce the frequency of permission-related errors when mounting
# in host directories for development.
if [ ! -z "${CHECKER_UID}" ]
then
    # Update the group id to match the specified one
    groupmod --gid $CHECKER_UID checker
    # Update the user id to match the specified one
    # Note: Switching the user home directory to avoid slow `chown` commands
    usermod --home /checker --uid $CHECKER_UID --gid $CHECKER_UID checker
    # Restore home directory (permissions for existing files won't be updated)
    usermod --home /home/checker checker
    # Chown any directories we absolutely must WRITE to with our new user id
    chown checker /home/checker/.config
fi

# Add opam switch environment variables
echo 'eval $(opam env --switch=/build --set-switch)' >> /home/checker/.bashrc
# Add the pre-installed python env to the PATH for convenience
echo 'export PATH=/build/.venv/bin:$PATH' >> /home/checker/.bashrc

# Start a shell as our user
gosu checker bash
