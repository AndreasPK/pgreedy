# Use an official Python runtime as a parent image
FROM haskell:8.4

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
COPY . /app

# Install any needed packages specified in requirements.txt
RUN cabal new-update

RUN cabal new-build all



# Run app.py when the container launches
#CMD ["bash", "/app"]