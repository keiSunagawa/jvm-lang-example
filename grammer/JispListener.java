// Generated from ./grammer/Jisp.g4 by ANTLR 4.7

    package me.kerfume.jisp.antlr;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link JispParser}.
 */
public interface JispListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link JispParser#error}.
	 * @param ctx the parse tree
	 */
	void enterError(JispParser.ErrorContext ctx);
	/**
	 * Exit a parse tree produced by {@link JispParser#error}.
	 * @param ctx the parse tree
	 */
	void exitError(JispParser.ErrorContext ctx);
	/**
	 * Enter a parse tree produced by {@link JispParser#value}.
	 * @param ctx the parse tree
	 */
	void enterValue(JispParser.ValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link JispParser#value}.
	 * @param ctx the parse tree
	 */
	void exitValue(JispParser.ValueContext ctx);
	/**
	 * Enter a parse tree produced by {@link JispParser#list}.
	 * @param ctx the parse tree
	 */
	void enterList(JispParser.ListContext ctx);
	/**
	 * Exit a parse tree produced by {@link JispParser#list}.
	 * @param ctx the parse tree
	 */
	void exitList(JispParser.ListContext ctx);
	/**
	 * Enter a parse tree produced by {@link JispParser#stmts}.
	 * @param ctx the parse tree
	 */
	void enterStmts(JispParser.StmtsContext ctx);
	/**
	 * Exit a parse tree produced by {@link JispParser#stmts}.
	 * @param ctx the parse tree
	 */
	void exitStmts(JispParser.StmtsContext ctx);
}