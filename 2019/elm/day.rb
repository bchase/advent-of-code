#!/usr/bin/env ruby

require 'pathname'
require 'fileutils'


class Day
  def self.init_files!(arg)
    new(arg).init
  end

  attr_reader :day
  def initialize(arg)
    @day = '%02d' % ARGV[0].to_i
  end

  def init
    mkdir!

    %w[ A B ].each do |ab|
      src  = template.gsub(/AB/, ab)
      sink = Pathname.new("src/Day#{day}/#{ab}.elm")

      sink.write src unless sink.exist?
    end
  end


  private

  def mkdir!
    FileUtils.mkdir_p "src/Day#{day}"
  end

  def template
    @template ||=
      Pathname.new('src/DayDAY.elm').read
        .gsub(/DAY/, day)
  end
end



Day.init_files!(ARGV[0])
