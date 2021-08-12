FROM ubuntu:21.10

RUN apt-get update
RUN apt-get install -y \
  curl xz-utils sudo \
  # brings /etc/services which seems to be necessary for Nix
  netbase

# https://aka.ms/vscode-remote/containers/non-root-user
ARG USER=checkeruser
ENV USER=$USER

ARG USER_UID=1000
ARG USER_GID=$USER_UID
RUN groupadd -f --gid $USER_GID $USER \
    && useradd --uid $USER_UID --gid $USER_GID -m $USER \
    && echo $USER ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USER \
    && chmod 0440 /etc/sudoers.d/$USER

RUN mkdir /nix
RUN chown -R $USER_UID:$USER_GID /nix /mnt

USER $USER

# install nix
RUN curl -L https://nixos.org/nix/install | sh

# install cachix and add tezos-checker cache
RUN bash -c '\
  source "$HOME/.nix-profile/etc/profile.d/nix.sh"; \
  nix-env -iA cachix -f https://cachix.org/api/v1/install && \
  cachix use tezos-checker'

# copy the files necessary for the nix evaluation. this is to prepopulate
# /nix/store with checkers dependencies.
RUN mkdir /tmp/checkernix

WORKDIR /tmp/checkernix
RUN mkdir -p nix/ bin/

COPY --chown=$USER_UID:$USER_GID *.nix                      ./
COPY --chown=$USER_UID:$USER_GID nix/*.nix nix/sources.json nix/

COPY --chown=$USER_UID:$USER_GID bin/ligo                   bin/

COPY --chown=$USER_UID:$USER_GID poetry.lock pyproject.toml ./
COPY --chown=$USER_UID:$USER_GID client/                    ./client
COPY --chown=$USER_UID:$USER_GID e2e/                       ./e2e

RUN bash -c 'source "$HOME/.nix-profile/etc/profile.d/nix.sh"; nix-shell --run :'

RUN rm -r /tmp/checkernix

# go back to the /mnt directory
WORKDIR /mnt

USER ROOT
COPY --chown=root:root nix/entrypoint.sh /entrypoint.sh

USER $USER
ENTRYPOINT ["/bin/bash", "/entrypoint.sh"]
CMD ["/bin/bash"]
