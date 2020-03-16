FROM hseeberger/scala-sbt:8u242_1.3.8_2.12.10 AS build

WORKDIR /src

COPY . /src

RUN sbt 'project toopHTTP4S' assembly

FROM openjdk:8u242-jre

COPY --from=build \
    /src/toop-http4s/target/scala-*/toop-http4s-assembly-*.jar \
    /src/toop-http4s.jar

WORKDIR /src

CMD java -jar toop-http4s.jar
