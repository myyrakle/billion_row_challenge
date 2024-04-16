cd java/basic && \
  javac Main.java && \
  native-image -O3 Main && \
  ./main && \
  cd .. && cd ..
