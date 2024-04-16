cd java/basic && \
  javac Main.java && \
  native-image -O3 -march=native --gc=G1 Main && \
  mv ./main ../../main && \
  cd .. && cd .. && \
  ./main
