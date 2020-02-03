package org.uwplse.liquid.spec

/* Created at 2/2/20 by zhen */
/**
 *
 * @param name
 * @param argNames TODO: relax to [[IdentifierPattern]]
 */
case class ConstraintDecl(name: String, argNames: List[String])