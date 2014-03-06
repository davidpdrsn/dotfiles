# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = "google-search"
  s.version = "1.0.3"

  s.required_rubygems_version = Gem::Requirement.new(">= 1.2") if s.respond_to? :required_rubygems_version=
  s.authors = ["TJ Holowaychuk"]
  s.date = "2012-08-29"
  s.description = "Google Search API"
  s.email = "tj@vision-media.ca"
  s.extra_rdoc_files = ["lib/google-search/item/base.rb", "lib/google-search/item/blog.rb", "lib/google-search/item/book.rb", "lib/google-search/item/image.rb", "lib/google-search/item/local.rb", "lib/google-search/item/news.rb", "lib/google-search/item/patent.rb", "lib/google-search/item/video.rb", "lib/google-search/item/web.rb", "lib/google-search/item.rb", "lib/google-search/response.rb", "lib/google-search/search/base.rb", "lib/google-search/search/blog.rb", "lib/google-search/search/book.rb", "lib/google-search/search/image.rb", "lib/google-search/search/local.rb", "lib/google-search/search/mixins/filter.rb", "lib/google-search/search/mixins/order_by.rb", "lib/google-search/search/mixins/safety_level.rb", "lib/google-search/search/mixins.rb", "lib/google-search/search/news.rb", "lib/google-search/search/patent.rb", "lib/google-search/search/video.rb", "lib/google-search/search/web.rb", "lib/google-search/search.rb", "lib/google-search/version.rb", "lib/google-search.rb", "README.rdoc", "tasks/docs.rake", "tasks/gemspec.rake", "tasks/spec.rake"]
  s.files = ["lib/google-search/item/base.rb", "lib/google-search/item/blog.rb", "lib/google-search/item/book.rb", "lib/google-search/item/image.rb", "lib/google-search/item/local.rb", "lib/google-search/item/news.rb", "lib/google-search/item/patent.rb", "lib/google-search/item/video.rb", "lib/google-search/item/web.rb", "lib/google-search/item.rb", "lib/google-search/response.rb", "lib/google-search/search/base.rb", "lib/google-search/search/blog.rb", "lib/google-search/search/book.rb", "lib/google-search/search/image.rb", "lib/google-search/search/local.rb", "lib/google-search/search/mixins/filter.rb", "lib/google-search/search/mixins/order_by.rb", "lib/google-search/search/mixins/safety_level.rb", "lib/google-search/search/mixins.rb", "lib/google-search/search/news.rb", "lib/google-search/search/patent.rb", "lib/google-search/search/video.rb", "lib/google-search/search/web.rb", "lib/google-search/search.rb", "lib/google-search/version.rb", "lib/google-search.rb", "README.rdoc", "tasks/docs.rake", "tasks/gemspec.rake", "tasks/spec.rake"]
  s.homepage = "http://github.com/visionmedia/google-search"
  s.rdoc_options = ["--line-numbers", "--inline-source", "--title", "Google-search", "--main", "README.rdoc"]
  s.require_paths = ["lib"]
  s.rubyforge_project = "google-search"
  s.rubygems_version = "2.0.3"
  s.summary = "Google Search API"

  if s.respond_to? :specification_version then
    s.specification_version = 3

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_runtime_dependency(%q<json>, [">= 0"])
    else
      s.add_dependency(%q<json>, [">= 0"])
    end
  else
    s.add_dependency(%q<json>, [">= 0"])
  end
end
