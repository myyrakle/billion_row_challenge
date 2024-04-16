cd java/basic && \
  javac Main.java && \
  native-image Main && \
  ls && \
  ./Main && \
  cd .. && cd ..