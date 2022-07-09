module Trie =
struct
  include Trie
  module Untagged = UntaggedTrie
end

module Language = Language
module Modifier = Modifier
module Scope = Scope
