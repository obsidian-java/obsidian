package obsidian.lang.java

import typings.vscode.mod as vscode
import typings.vscode.anon.Dispose
import typings.vscode.Thenable

import scala.collection.immutable
import scala.util.*
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.UndefOr

import obsidian.lang.java.Obsidian.*
import obsidian.lang.java.scalangj.Parser.parseNonEmptyCompilationUnit
import obsidian.lang.java.scalangj.Pretty.prettyPrint

import concurrent.ExecutionContext.Implicits.global

object extension {
    @JSExportTopLevel("activate")
    def activate(context: vscode.ExtensionContext): Unit = {
        println(
            """Congratulations, your extension "obsidian" is now active!"""
        )

        def showHello(): js.Function1[Any, Any] =
            (arg) => {
                vscode.window.showInputBox().toFuture.onComplete {
                    case Success(input) => vscode.window.showInformationMessage(s"Hello World $input!")
                    case Failure(e)     => println(e.getMessage)
                }
            }
        
        def obsidian(context: vscode.ExtensionContext): js.Function1[Any, Any] =
            (arg) => {
                vscode.window.activeTextEditor.toOption match {
                case Some(editor) => {
                    editor.edit(editorBuilder =>
                        editorBuilder.replace(
                            vscode.Range(
                                editor.selection.start.line,
                                editor.selection.start.character,
                                editor.selection.`end`.line,
                                editor.selection.`end`.character
                            ),
                            parseNonEmptyCompilationUnit(
                                editor.document.getText(
                                    vscode.Range(
                                        editor.selection.start.line,
                                        editor.selection.start.character,
                                        editor.selection.`end`.line,
                                        editor.selection.`end`.character
                                    )
                                )
                            ) match {
                                case Left(error_msg) => {
                                    /*vscode.window.createOutputChannel("Obsidian").appendLine(error_msg)
                                    editor.document.getText(
                                        vscode.Range(
                                            editor.selection.start.line,
                                            editor.selection.start.character,
                                            editor.selection.`end`.line,
                                            editor.selection.`end`.character
                                        )
                                    )*/
                                    println(error_msg)
                                    error_msg
                                }
                                case Right(cu)       => prettyPrint(run(cu))
                            }
                        )
                    )
                }
                case None =>
                    vscode.window.showInformationMessage("No active editor")
                }
            }

        val commands = List(
            ("extension.helloWorld", showHello()),
            ("extension.obsidian", obsidian(context))
        )

        commands.foreach { case (name, fun) =>
            context.subscriptions.push(
                vscode.commands
                .registerCommand(name, fun)
                .asInstanceOf[Dispose]
            )
        }

    }
}