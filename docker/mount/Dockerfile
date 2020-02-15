FROM tensorflow/haskell:v0
WORKDIR /app
RUN apt-get install -y git
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN PATH=$PATH:/root/.local/bin >> ~/.bashrc
