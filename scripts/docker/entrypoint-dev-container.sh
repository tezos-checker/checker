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

# If a specific UID is supplied, make sure that the container user has that UID.
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
    # Note: This allows us to write to home dir but not to pre-existing files there
    chown checker /home/checker
    chown checker /home/checker/.config
fi

# To support docker in docker, need to ensure that the docker group in the container
# has the GID as the docker group on the host (otherwise you can't call docker commands
# in the container).
if [ ! -z "${DOCKER_GID}" ]
then
    # Update the group id to match the specified one
    groupmod --gid $DOCKER_GID docker
fi

# Add opam switch environment variables
echo 'eval $(opam env --switch=/build --set-switch)' >> /home/checker/.bashrc
# Add the pre-installed python env to the PATH for convenience
echo 'export PATH=/build/.venv/bin:$PATH' >> /home/checker/.bashrc

# Start a shell as our user
gosu checker bash
