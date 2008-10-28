require 'buildr/scala'

repositories.remote << 'http://www.ibiblio.org/maven2'
repositories.remote << 'http://scala-tools.org/repo-releases'

define 'scala_stm' do
  project.version = '0.1.0'
  project.group = 'com.codecommit'

  test.with 'com.codecommit:collection:jar:0.1.0'
  
  package :jar
end
