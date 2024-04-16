cd java/basic && \
  javac Main.java && \
  native-image -O3 -march=native Main && \
  mv ./main ../../main && \
  cd .. && cd .. && \
  ./main
