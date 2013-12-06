namespace :java do
  desc 'Setup junit'
  task :junit do
    unless File.directory? "junit"
      curl "http://search.maven.org/remotecontent?filepath=junit/junit/4.11/junit-4.11.jar"
      curl "http://search.maven.org/remotecontent?filepath=org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar"

      jar "junit-4.11.jar"
      jar "hamcrest-core-1.3.jar"
    end
  end
end

def jar path
  system "jar xf #{path}"
end

def curl path
  system "curl -O #{path}"
end
