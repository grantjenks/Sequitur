////////////////////////////////////////////////////////////////////////////////
// Grant Jenks (c) 2009
//
// This program is based off:
//   Nevill-Manning, C.G. and Witten, I.H. (1997)
//   "Identifying Hierarchical Structure in Sequences: A linear-time algorithm"
//   Journal of Artificial Intelligence Research, 7, 67-82.
//
// A simpler version of this program is available at
//   http://sequitur.info/sequitur_simple.cc

////////////////////////////////////////////////////////////////////////////////
// Includes (depends on STL and BOOST)

#include <iostream>
#include <fstream>
#include <string>
#include <functional>
#include <iterator>
#include <algorithm>
#include <list>
#include <stack>

#include <boost/format.hpp>
#include <boost/unordered_set.hpp>
#include <boost/unordered_map.hpp>

////////////////////////////////////////////////////////////////////////////////

using namespace std;

////////////////////////////////////////////////////////////////////////////////
// Forward declarations

class Symbol;
class Rule;

void dump_rule(Rule * rule);
void dump_digrams();

static void validate(Rule * start, string value, int stringCount,
		     bool forceCheck);
static void debug(bool expr);

////////////////////////////////////////////////////////////////////////////////
// Digram management declarations

struct DigramEquals : std::binary_function<Symbol *, Symbol *, bool>
{
  bool operator()(const Symbol * lhs, const Symbol * rhs) const;
};

struct DigramHash : std::unary_function<Symbol *, std::size_t>
{
  std::size_t operator()(const Symbol * symbol) const;
};

typedef boost::unordered_set<Symbol *, DigramHash, DigramEquals> DigramSet;

static DigramSet _digrams;

////////////////////////////////////////////////////////////////////////////////

class Rule
{
private:

  // The guard node in the linked list of symbols that make up the rule.
  // It points forward to the first symbol in the rule, and backwards
  // to the last symbol in the rule. Its own value points to the rule data
  // structure, so that symbols can find out which rule they're in.

  Symbol * _guard;

  // Number of rules (used to uniquely identify rules).

  static int num_rules;
  int next_id() { return ++num_rules; }

public:

  //  Keeps track of the number of times the rule is used in the grammar.

  int _freq;

  // Uniquely identifies this rule.

  int _id;

public:
  Rule();
  ~Rule();

public:
  void reuse() { _freq ++; }
  void deuse() { _freq --; }

  Symbol * first();
  Symbol * last();
};

int Rule::num_rules = 0;

////////////////////////////////////////////////////////////////////////////////

class Symbol
{

public:

  enum Type
    {
      NonTerminal,
      Terminal,
      Guard
    };

  string _value;
  Type _type;
  Rule * _rule;
  Symbol * _next;
  Symbol * _prev;


public:

  // Initializes a new symbol. If it is non-terminal, increments the reference
  // count of the corresponding rule.

  Symbol(string value)
  {
    _value = value;
    _type = Terminal;
    _rule = 0;
    _next = 0;
    _prev = 0;
  }

  Symbol(Rule *r, Symbol::Type type)
  {
    _value = boost::str(boost::format("NT%1%") % r->_id);
    _type = type;
    r->reuse();
    _rule = r;
    _next = 0;
    _prev = 0;
  }

  // Links two symbols together, removing any old digram from the hash table.

  static void join(Symbol *left, Symbol *right)
  {
    debug(left != 0 && right != 0);

    if (left->_next)
      {
	left->delete_digram();

	// This is to deal with triples, where we only record the second
	// pair of the overlapping digrams. When we delete the second pair,
	// we insert the first pair into the hash table so that we don't
	// forget about it.  e.g. abbbabcbb

	if (right->_prev && right->_next &&
	    right->_value == right->_prev->_value &&
	    right->_value == right->_next->_value)
	  {
	    _digrams.insert(right);
	  }

	if (left->_prev && left->_next &&
	    left->_value == left->_next->_value &&
	    left->_value == left->_prev->_value)
	  {
	    _digrams.insert(left->_prev);
	  }
      }

    left->_next = right; right->_prev = left;
  }

  // Cleans up for symbol deletion: removes hash table entry and decrements
  // rule reference count.

  ~Symbol()
  {
    join(_prev, _next);

    if (!is_guard())
      {
	delete_digram();

	if (is_nt()) rule()->deuse();
      }
  }

  // Inserts a symbol after this one.

  void insert_after(Symbol *y)
  {
    debug(y != 0);

    join(y, _next);
    join(this, y);
  }

  // Removes the digram from the hash table.

  void delete_digram()
  {
    if (is_guard() || _next->is_guard()) return;

    DigramSet::iterator i = _digrams.find(this);

    if (i == _digrams.end()) return;

    if ((*i) == this) _digrams.erase(this);
  }

  bool is_guard() { return (_type == Guard); }
  bool is_nt() { return (_type == NonTerminal); }
  bool is_terminal() { return (_type == Terminal); }

  // Assuming this is a non-terminal, returns the corresponding rule.

  Rule *rule()
  {
    debug(_type != Terminal);
    return _rule;
  }

  void substitute(Rule *r);
  static void match(Symbol *s, Symbol *m);

  // Checks a new digram. If it appears elsewhere, deals with it by calling
  // match(), otherwise inserts it into the hash table.

  bool check()
  {
    if (is_guard() || _next->is_guard()) return false;

    DigramSet::iterator i = _digrams.find(this);

    if (i == _digrams.end())
      {
	_digrams.insert(this);
	return false;
      }

    Symbol *x = (*i);

    if (x->_next != this) match(this, x);

    return true;;
  }

  void expand();

  void point_to_self() { join(this, this); }
};

////////////////////////////////////////////////////////////////////////////////
// Digram management definitions

static bool equals_helper(const Symbol * lhs, const Symbol * rhs)
{
  if (lhs->_type != rhs->_type)
    {
      return false;
    }
  else
    {
      if (lhs->_type == Symbol::Terminal)
	{
	  return (lhs->_value == rhs->_value);
	}
      else if (lhs->_type == Symbol::NonTerminal)
	{
	  return (lhs->_rule->_id == rhs->_rule->_id);
	}
      else
	{
	  debug(false);
	  return false;
	}
    }
}

bool
DigramEquals::operator()(const Symbol * lhs, const Symbol * rhs) const
{
  debug(lhs->_type != Symbol::Guard);
  debug(rhs->_type != Symbol::Guard);
  debug(lhs->_next != 0);
  debug(rhs->_next != 0);

  bool thisMatch = equals_helper(lhs, rhs);

  if (!thisMatch) return false;

  bool nextMatch = equals_helper(lhs->_next, rhs->_next);

  return nextMatch;
}

static size_t hash_helper(const Symbol * symbol, size_t seed)
{
  debug(symbol != 0);

  if (symbol->_type == Symbol::Terminal)
    {
      boost::hash_combine(seed, symbol->_value);
    }
  else if (symbol->_type == Symbol::NonTerminal)
    {
      boost::hash_combine(seed, symbol->_rule->_id);
    }
  else
    {
      debug(false);
    }

  return seed;
}

std::size_t
DigramHash::operator() (const Symbol * symbol) const
{
  debug(symbol != 0);
  debug(symbol->_next != 0);

  size_t seed = 0;

  seed = hash_helper(symbol, seed);

  seed = hash_helper(symbol->_next, seed);

  return seed;
}

////////////////////////////////////////////////////////////////////////////////
// Program entry point.

int main(int argc, char** argv)
{
  // __debugbreak();

  ifstream file;
  istream_iterator<string> input;

  if (argc == 2)
    {
      file.open(argv[1]);
      if (file.fail())
	{
	  printf("Error opening file.");
	  return 1;
	}
      input = istream_iterator<string>(file);
    }
  else
    {
      input = istream_iterator<string>(cin);
    }

  Rule * S = new Rule();
  _digrams.clear();

  int count = 0;

  for(; input != istream_iterator<string>(); input++)
    {
      S->last()->insert_after(new Symbol(*input));
      S->last()->_prev->check();

      count++;
      validate(S, (*input), count, false);
    }

  validate(S, string(""), count, true);

  printf("Printing Digrams:\n");
  dump_digrams();

  printf("Printing Rules:\n");
  dump_rule(S);

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
// Definitions that require both Symbol and Rule declarations.

Rule::Rule()
{
  _id = next_id();
  _guard = new Symbol(this, Symbol::Guard);
  _guard->point_to_self();
  _freq = 0;
}

Rule::~Rule()
{
  delete _guard;
}

Symbol *Rule::first()
{
  debug(_guard != 0);
  return _guard->_next;
}

Symbol *Rule::last()
{
  debug(_guard != 0);
  return _guard->_prev;
}

// This symbol is the last reference to its rule. It is deleted, and the
// contents of the rule substituted in its place.

void Symbol::expand()
{
  Symbol *left = _prev;
  Symbol *right = _next;
  Symbol *first = rule()->first();
  Symbol *last = rule()->last();

  delete rule();

  DigramSet::iterator i = _digrams.find(this);

  if (i == _digrams.end()) assert(false);

  if ((*i) == this) _digrams.erase(this);

  delete this;

  join(left, first);
  join(last, right);

  _digrams.insert(last);
}

// Replace a digram with a non-terminal.

void Symbol::substitute(Rule *rule)
{
  Symbol *prev = _prev;

  delete prev->_next;
  delete prev->_next;

  prev->insert_after(new Symbol(rule, NonTerminal));

  if (!prev->check()) prev->_next->check();
}

// Deal with a matching digram.

void Symbol::match(Symbol *symbol, Symbol *match)
{
  Rule *rule;

  // Reuse an existing rule.

  if (match->_prev->is_guard() && match->_next->_next->is_guard())
    {
      rule = match->_prev->rule();
      symbol->substitute(rule);
    }
  else
    {
      // Create a new rule.

      rule = new Rule();

      if (symbol->is_nt())
	rule->last()->insert_after(new Symbol(symbol->rule(), NonTerminal));
      else
	rule->last()->insert_after(new Symbol(symbol->_value));

      if (symbol->_next->is_nt())
	rule->last()->insert_after(new Symbol(symbol->_next->rule(), NonTerminal));
      else
	rule->last()->insert_after(new Symbol(symbol->_next->_value));

      match->substitute(rule);
      symbol->substitute(rule);

      _digrams.insert(rule->first());
    }

  // Check for an underused rule.

  if (rule->first()->is_nt() && rule->first()->rule()->_freq == 1)
    rule->first()->expand();
}

////////////////////////////////////////////////////////////////////////////////
// Correctness Helpers

static void validate(Rule * start, string value, int stringCount,
		     bool forceCheck)
{
#if CHECK > 0

  debug(start != 0);

  // Static allocation.

  static list<string> input_strings;

  // Always add the next string value to our list of strings.

  if (!forceCheck)
    {
      input_strings.push_back(value);
    }

  // Check only when stringCount is a multiple of CHECK or when forceCheck
  // is true.

  if (stringCount % CHECK != 0 && !forceCheck)
    {
      return;
    }

  // Things to check:
  // 1. Expansion matches input.
  // 2. No pair of adjacent symbols appears more than once in the grammar.
  // 3. Every rule is used more than once.
  
  boost::unordered_map<string, int> ruleUsage;

  // Walk over input string starting with the "start" rule. Normally
  // this would be done with a recursive algorithm (as with dump_rules)
  // but here "validate" is static.

  list<string>::iterator isi = input_strings.begin();

  stack<Symbol *> symbol_stack;
  symbol_stack.push(start->first());

  while(!symbol_stack.empty())
    {
      Symbol * current = symbol_stack.top();
      symbol_stack.pop();
      
      debug(current != 0);

      if (current->is_terminal())
	{
	  // 1. Expansion matches input.

	  debug(isi != input_strings.end());
	  debug((*isi) == current->_value);

	  isi++;
	  symbol_stack.push(current->_next);
	}
      else if (current->is_nt())
	{
	  string id = current->_value;
	  boost::unordered_map<string, int>::iterator rui = ruleUsage.find(id);
	  if (rui == ruleUsage.end())
	    {
	      ruleUsage[id] = 1;
	    }
	  else
	    {
	      ruleUsage[id] = ruleUsage[id] + 1;
	    }
	  debug(ruleUsage.count(id) == 1);

	  symbol_stack.push(current->_next);
	  symbol_stack.push(current->_rule->first());
	}
      else if (current->is_guard())
	{
	  // Do nothing.
	}
      else
	{
	  debug(false);
	}
    }

  // Make sure we're now at the end of the input strings list.

  debug(isi == input_strings.end());

  // 3. Every rule is used more than once.

  for (boost::unordered_map<string, int>::iterator rui = ruleUsage.begin();
       rui != ruleUsage.end();
       rui++)
    {
      debug(rui->second > 1);
    }

#endif
}

static void debug(bool expr)
{
  // It makes no sense to define both BREAK and ASSERT.

#if defined(BREAK)

  if (!expr) __debugbreak();

#elif defined(ASSERT)

  assert(expr);

#endif
}

////////////////////////////////////////////////////////////////////////////////
// Dump functions

static int indent = 0;
void dump_rule(Rule * rule)
{
  for (int i = 0; i < indent; i++) printf(" ");
  printf("%i -> (count %i)\n", rule->_id, rule->_freq);
  indent += 3;
  Symbol * symbol = rule->first();
  do {
    for (int i = 0; i < indent; i++) printf(" ");
    printf("%s\n", symbol->_value.c_str());
    if (symbol->is_nt())
      {
	dump_rule(symbol->rule());
      }
    symbol = symbol->_next;
  } while (!symbol->is_guard());
  indent -= 3;
}

static void dump_digrams()
{
  for (DigramSet::iterator i = _digrams.begin();
       i != _digrams.end();
       i++)
    {
      Symbol * symbol = (*i);
      printf("   %s , %s\n",
	     symbol->_value.c_str(),
	     symbol->_next->_value.c_str());
    }
}
