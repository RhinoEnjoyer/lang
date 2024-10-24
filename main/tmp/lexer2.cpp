#include "./lexer.hpp"
#include "../codes/codes.hpp"
#include "lex.yy.cpp"

#include <cwctype>
#include <fstream>
#include <sstream>
#include <utility>

  bool is_limitset(const tokc type){
    return SYMETRICAL_WRAP_FIRST < type && type < SYMETRICAL_WRAP_LAST;
  }
    
  snode stmtgroups(
    std::vector<snode>::iterator it,
    const std::vector<snode>::iterator end,
    tokc  type
  ){
    std::vector<snode> buffer;
    std::vector<snode> stack;
    bool s = false;
    while(it != end){
      //THIS LINE BASICALY CONTROLS OUR LIMITER
      if(it->type() == SCOLON){
        //IMPORTANT LINE
        //buffer.push_back(*it);
        stack.push_back(snode::wrap(tokc::GROUP,buffer));
        buffer.clear();
        s = true;
      }else if(is_limitset(it->type())){
        buffer.push_back(stmtgroups(it->child().begin(),it->child().end(),it->type())); 
      }else{
        buffer.push_back(*it);
      }
      ++it;
    }
    
    if(buffer.size() > 0){
      if(!s){
        const snode n = snode::wrap(tokc::GROUP,{buffer.begin(),buffer.end()});
        stack.push_back(n);
      }else{
        stack.push_back(snode::wrap(GROUP,buffer));
      }
    }
   
    return snode::wrap(type,stack);
  }


// #include <optional>

//     static std::optional<std::pair<std::set<std::array<tokc,3>>::iterator,size_t>> find(
//       const std::set<std::array<tokc,3>>& groups,
//       tokc tok
//     ){
//       for(std::array<tokc,3> g: groups){
//         size_t i = 0;
//         for(tokc& v: g){
//           if(v == tok){
//             return {{groups.find(g),i}};
//           }
//           i++;
//         }
//       }

//       return {};
//     }
//     snode groups_impl(
//       std::vector<snode>::iterator& it,
//       const std::vector<snode>::iterator end,
//       tokc type,
//       const std::set<std::array<tokc,3>>& group_sets,
//       std::map<tokc,std::vector<snode>>& open,
//       std::map<tokc,std::vector<snode>>& stray
//     ){
//       std::vector<snode> buffer;

//       const auto bi = it -1;
      
//       while(it < end){
//         const auto opt = find(group_sets,it->type());
//         if(opt.has_value()){
//           const size_t i = opt.value().second;
//           const auto& arr = (*opt.value().first);
//           if(i == 1){
//             it++;
//             //open
//             open[arr.at(0)].push_back(*(it-1));
//             buffer.push_back(groups_impl(it, end, arr.at(0), group_sets, open,stray));
//           }
//           else if(i == 2){
//             //close
//             const bool h = find(group_sets,type).has_value();
//             if(!h){
//               stray[arr.at(0)].push_back(*it);
//             }else if(open[arr.at(0)].size() > 0){
//                 open[arr.at(0)].pop_back();
//                 //TODO: THIS PART NEEDS TO BE ADDRESSED
//                 // IT WORKS FOR NOW
//                 // AND IT WORKS WELL
//                 // IT MIGHT NOT CAUSE PROBLEMS
//                 // BUT IT IS GOOD TO MARK IT
//                 {
//                   snode r = snode::wrap(type,buffer);
//                   r.index(bi->index());
//                   if(r.child(0).type() == EMPTY)
//                     r.child(0).index(r.index());
//                   //std::cout << code_str(r.type())  << " " << loct::info(r->mloc) << '\n';
//                   return r;
//                 }
                
//             }else{
//               stray[arr.at(0)].push_back(*it);
//             }

//           }
//         }
//         else
//           buffer.push_back(*it);
//         it++;
//       }
//       //std::cout << snode::infostr(*(buffer.end()-1)) << '\n';

//       return snode::wrap(type,buffer);
//     };



//    snode groups(
//       std::vector<snode>::iterator it,
//       std::vector<snode>::iterator end,
//       tokc type = UNIT
//     ){
//       const std::set<std::array<tokc ,3>> group_sets = {
//         {PARENS,LPAREN,RPAREN},
//         {BRACES,LBRACE,RBRACE},
//         {CBRACES,LCBRACE,RCBRACE},
//         {DBRACES,LDBRACE,RDBRACE},
//         {ANGLES,LANGLE,RANGLE},
//       };
//       std::map<tokc,std::vector<snode>> open;
//       std::map<tokc,std::vector<snode>> stray;
//       const snode n = groups_impl(it,end,type,group_sets,open,stray);

//       bool open_flag = false;
//       for(const auto& o: open){
//         for(const auto& g: o.second){
//           std::cerr << "Failed to close: " << snode::info(g) << '\n';
//           open_flag = true;
//         }
//       }
//       for(const auto& s: stray){
//         for(const auto& g: s.second){
//           std::cerr << "Failed to open: " << snode::info(g) << '\n';
//           open_flag = true;
//         }
//       }
//       if(open_flag)
//         std::abort();

//       return snode::wrap(type,{n.child().begin(),n.child().end()});
//     };


namespace symetrical{

bool iswrap(const tokc ctok){return SYMETRICAL_WRAP_FIRST  < ctok && ctok < SYMETRICAL_WRAP_LAST;}
bool isleft(const tokc ctok){return SYMETRICAL_LEFT_FIRST  < ctok && ctok < SYMETRICAL_LEFT_LAST;}
bool isright(const tokc ctok){return SYMETRICAL_RIGHT_FIRST  < ctok && ctok < SYMETRICAL_RIGHT_LAST;}
constexpr tokc calc(const tokc ctg1st, const tokc tg1st, const tokc tok){return static_cast<tokc>((tok - ctg1st) + tg1st);};

tokc getright(const tokc ctok){
  if(iswrap(ctok))
    return calc(SYMETRICAL_WRAP_FIRST,SYMETRICAL_RIGHT_FIRST,ctok);
  else if(isleft(ctok))
    return calc(SYMETRICAL_LEFT_FIRST,SYMETRICAL_RIGHT_FIRST,ctok);
  return ERROR;
}
tokc getleft(const tokc ctok){
  if(iswrap(ctok))
    return calc(SYMETRICAL_WRAP_FIRST,SYMETRICAL_LEFT_FIRST,ctok);
  else if(isright(ctok))
    return calc(SYMETRICAL_RIGHT_FIRST,SYMETRICAL_LEFT_FIRST,ctok);
  return ERROR;
}
tokc getwrap(const tokc ctok){
  if(isright(ctok))
    return calc(SYMETRICAL_RIGHT_FIRST,SYMETRICAL_WRAP_FIRST,ctok);
  else if(isleft(ctok))
    return calc(SYMETRICAL_LEFT_FIRST,SYMETRICAL_WRAP_FIRST,ctok);
  return ERROR;
}

}
bool handle_nest(Lexer& lexer){
  tokc tok;
  std::vector<snode>* vecparent;
  snode* last;
  bool end = false;

  AGAIN:
    if(end) 
      return true;

    tok = (tokc)lexer.lex();
    vecparent = lexer.nodes;
    last = (vecparent->end()-1).base();

    if(symetrical::isleft(tok)){
      last->type(symetrical::getwrap(tok));
      lexer.nodes = &last->child();
      handle_nest(lexer);
      lexer.nodes = vecparent;
      goto AGAIN;
    }else if(symetrical::isright(tok)){
      lexer.nodes->pop_back();
      if(lexer.nodes->size() == 0)
        lexer.nodes->push_back(snode::empty());
      return false;
    }else if(tok == tokc::ENDTKN){
      return true;
    }
    __builtin_unreachable();
}


namespace frontend{
  namespace lexer{
    lexer_return lex(std::ifstream& src_file){
        snode unit = snode::create(UNIT);
        auto lexer = Lexer(&unit.child(),src_file,std::cerr);
        handle_nest(lexer);
        // unit = groups(unit.child().begin(), unit.child().end());
        unit = stmtgroups(unit.child().begin(),unit.child().end(),UNIT);
        return {unit, /* std::move(lexer.toks), */ std::move(lexer.locs)};
    }
  }
}
