
task :default => :usage

task :usage do
  puts 'Try one of these commands:'
  sh %{rake -T}
end

desc 'This builds the summary descriptions for the CCSWG12.'
file 'ccswg12.html' => ['ccswg12.md'] do |t|
  sh %(pandoc --standalone --html5 --smart --output=#{t.name} --css=stylesheets/screen.css #{t.prerequisites.join(' ')})
end

namespace :compass do
  desc 'This watches the SASS files and recompiles them automatically.'
  task :watch do
    sh %(compass watch .)
  end

  desc 'This compiles all the SASS files.'
  task :build do
    sh %(compass compile .)
  end
end

multitask :watch => ['compass:watch']

