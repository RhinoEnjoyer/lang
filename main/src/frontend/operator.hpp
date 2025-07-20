  enum class op_type_e : std::int8_t { UNARY = 1, BINARY = 2, TERNARY = 3 };
  enum class op_assoc_e : std::int8_t{ LEFT, RIGHT };
  enum class op_operation_e : std::int8_t{
    PLUS = 0,
    MINUS,
    MULT,
    DIV,
    MOD,
    PLUSPLUS,
    MINUSMINUS,
    EQ,
    NEQ,
    GREATER,
    LESS,
    LEQ,
    GEQ,
    NOT,
    AND,
    XOR,
    OR,
    SLEFT,
    SRIGHT,
    ASSIGN,
    PLUSASSIGN,
    MINUSASSIGN,
    MULTASSIGN,
    DIVASSIGN,
    PIPE,
    DIAMOND,
    NEG,
    DEREF,
    ADDRESS,
    AS,
    last
  };

  enum class op_pos_e : std::int8_t{
    PREFIX,
    INFIX,
    POSTFIX,
   };

  struct op_meta_t {
    const op_operation_e op;
    const op_type_e type;
    const op_assoc_e assoc;
    const op_pos_e pos;
    const std::int8_t prec;
    const bool has_payload;
  };

  static constexpr auto op_table =
      std::array<op_meta_t, static_cast<size_t>(op_operation_e::last)>{
// Arithmetic
          op_meta_t{op_operation_e::PLUS,       op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  8,  false},
          op_meta_t{op_operation_e::MINUS,      op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  8,  false},
          op_meta_t{op_operation_e::MULT,       op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  9,  false},
          op_meta_t{op_operation_e::DIV,        op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  9,  false},
          op_meta_t{op_operation_e::MOD,        op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  9,  false},
          op_meta_t{op_operation_e::PLUSPLUS,   op_type_e::UNARY,  op_assoc_e::LEFT , op_pos_e::POSTFIX,  12, false},
          op_meta_t{op_operation_e::MINUSMINUS, op_type_e::UNARY,  op_assoc_e::LEFT , op_pos_e::POSTFIX,  12, false},
// Comparison
          op_meta_t{op_operation_e::EQ,         op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  5,  false},
          op_meta_t{op_operation_e::NEQ,        op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  5,  false},
          op_meta_t{op_operation_e::GREATER,    op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  6,  false},
          op_meta_t{op_operation_e::DIAMOND,    op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  6,  false},
          op_meta_t{op_operation_e::LESS,       op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  6,  false},
          op_meta_t{op_operation_e::LEQ,        op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  6,  false},
          op_meta_t{op_operation_e::GEQ,        op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  6,  false},
// Binary logic & shift
          op_meta_t{op_operation_e::NOT,        op_type_e::UNARY,  op_assoc_e::RIGHT, op_pos_e::PREFIX, 10, false},
          op_meta_t{op_operation_e::AND,        op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  4,  false},
          op_meta_t{op_operation_e::XOR,        op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  3,  false},
          op_meta_t{op_operation_e::OR,         op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  2,  false},
          op_meta_t{op_operation_e::SLEFT,      op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  7,  false},
          op_meta_t{op_operation_e::SRIGHT,     op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  7,  false},
// Assignment
          op_meta_t{op_operation_e::ASSIGN,     op_type_e::BINARY, op_assoc_e::RIGHT, op_pos_e::INFIX, -99, false},
          op_meta_t{op_operation_e::PLUSASSIGN, op_type_e::BINARY, op_assoc_e::RIGHT, op_pos_e::INFIX, -99, false},
          op_meta_t{op_operation_e::MINUSASSIGN,op_type_e::BINARY, op_assoc_e::RIGHT, op_pos_e::INFIX, -99, false},
          op_meta_t{op_operation_e::MULTASSIGN, op_type_e::BINARY, op_assoc_e::RIGHT, op_pos_e::INFIX, -99, false},
          op_meta_t{op_operation_e::DIVASSIGN,  op_type_e::BINARY, op_assoc_e::RIGHT, op_pos_e::INFIX, -99, false},
// Misc
          op_meta_t{op_operation_e::PIPE,       op_type_e::BINARY, op_assoc_e::LEFT , op_pos_e::INFIX,  0,  false},
          op_meta_t{op_operation_e::NEG,        op_type_e::UNARY,  op_assoc_e::RIGHT, op_pos_e::PREFIX, 11, false},
          op_meta_t{op_operation_e::ADDRESS,    op_type_e::UNARY,  op_assoc_e::RIGHT, op_pos_e::PREFIX, 11, false},
          op_meta_t{op_operation_e::AS,         op_type_e::UNARY,  op_assoc_e::RIGHT, op_pos_e::PREFIX, 10,  true},
      };

