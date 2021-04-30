#!/usr/bin/env ruby

require 'etc'
require 'json'
require 'open3'
require 'optparse'

CONTRACT_MAIN="#{__dir__}/../generated/michelson/main.tz"
FUNCTIONS=JSON.parse(File.read("#{__dir__}/../generated/michelson/functions.json"), object_class: OpenStruct)

args = {}
OptionParser.new do |parser|
  parser.on("--ctez ADDRESS")
  parser.on("--oracle ADDRESS")
end.parse!(into: args)

raise OptionParser::MissingArgument, "--ctez" if args[:ctez].nil?
raise OptionParser::MissingArgument, "--oracle" if args[:oracle].nil?

output, exit_status = Open3.capture2("tezos-client", "show", "address", "bob")
exit_status.success? or raise "tezos-client show address failed:\n#{output}"
self_address = output.scan(/^Hash: (\S*)$/)[0][0]

puts "Using address: #{self_address}"

storage = "(Pair {} (Right \"#{self_address}\"))"

puts "Deploying the main contract."
system(
  "tezos-client",
  "--wait", "1",
  "originate",
  "contract", "checker",
  "transferring", "0",
  "from", self_address,
  "running", CONTRACT_MAIN,
  "--init", storage,
  "--no-print-source",
  "--burn-cap", "5",
  "--force",
)
$?.success? or raise

puts "Deploying the endpoints."
# We can deploy multiple endpoints with the same operation, given the operation itself is less than 16
# kilobytes (on Edo). So we arrange them into chunks here.
#
# To approximate size, we assume that the packed representation is twice the length of the binary
# representation.
deploy_chunks =
  FUNCTIONS
    .lazy_functions
    .map { |s| s.chunks.map { |chunk| "Pair #{s.fn_id} 0x#{chunk}" } }
    .sort_by { |i| i.map(&:length).sum }
    .flatten
    .reduce([[]]) { |memo, curr|
       last_size = memo.last.map(&:length).sum
       if last_size + curr.length < 15000*2
         memo.last.append(curr)
       else
         memo.append([curr])
       end
       memo
    }

deploy_chunks.each_with_index do |chunk, i|
  puts "Deploying chunk #{i+1} of #{deploy_chunks.length}..."

  transfers = chunk.map
    .map { |arg|
      { destination: "checker", amount: "0", entrypoint: "deployFunction", arg: arg  }
    }
  stdout, stderr, exit_status = Open3.capture3(
    "tezos-client",
    "--wait", "1",
    "multiple", "transfers", "from", self_address,
    "using", JSON.pretty_generate(transfers),
    "--burn-cap", "10",
  )
  exit_status.success? or raise "deployFunction failed.\nstdout:\n#{stdout}\nstderr\n#{stderr}"
end

puts "Sealing the contract."
system(
  "tezos-client",
  "--wait", "1",
  "call", "checker",
  "from", self_address,
  "--entrypoint", "sealContract",
  "--arg", "(Pair \"#{args[:oracle]}\" \"#{args[:ctez]}\")",
  "--burn-cap", "5",
  "--no-print-source",
)
$?.success? or raise

puts "Done."
