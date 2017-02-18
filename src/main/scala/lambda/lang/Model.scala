package lambda.lang

object Model {

    val root =
        module (
            'lambda -> module (
                'sdk -> module (
                    'test -> ref ('lambda, 'sdk, 'test)
                ),
                'lang -> module (
                    'name -> record (
                        'words -> apply (
                            ref ('lambda, 'sdk, 'seq),
                            ref ('lambda, 'lang, 'name)
                        )
                    ),
                    'exp -> taggedUnion (
                        'const -> record (
                            'data -> ref ('lambda, 'lang, 'data)
                        ),
                        'var -> record (
                            'name -> ref ('lambda, 'lang, 'name)
                        ),
                        'apply -> record (
                            'fun -> ref ('lambda, 'lang, 'exp),
                            'args -> apply (
                                ref ('lambda, 'sdk, 'seq),
                                ref ('lambda, 'lang, 'exp)
                            )
                        ),
                        'lambda -> record (
                            'arg_names -> apply (
                                ref ('lambda, 'sdk, 'seq),
                                ref ('lambda, 'lang, 'name)
                            ),
                            'body -> ref ('lambda, 'lang, 'exp)
                        ),
                        'select -> record (
                            'scope -> ref ('lambda, 'lang, 'exp),
                            'path -> apply (
                                ref ('lambda, 'sdk, 'seq),
                                ref ('lambda, 'lang, 'name)
                            )
                        ),
                        'let -> record (
                            'bindings -> apply (
                                ref ('lambda, 'sdk, 'seq),
                                tuple (
                                    ref ('lambda, 'lang, 'name),
                                    ref ('lambda, 'lang, 'exp)
                                )
                            ),
                            'in -> ref ('lambda, 'lang, 'exp)
                        )
                    ),
                    'data -> taggedUnion (
                        'type -> taggedUnion (
                            'module -> record (
                                'child_types -> apply (
                                    ref ('lambda, 'sdk, 'map),
                                    ref ('lambda, 'lang, 'name),
                                    ref ('lambda, 'lang, 'type_exp)
                                )
                            ),
                            'record -> record (
                                'fields -> apply (
                                    ref ('lambda, 'sdk, 'seq),
                                    tuple (
                                        ref ('lambda, 'lang, 'name),
                                        ref ('lambda, 'lang, 'type_exp)
                                    )
                                )
                            ),
                            'tagged_union -> record (
                                'sub_types -> apply (
                                    ref ('lambda, 'sdk, 'map),
                                    ref ('lambda, 'lang, 'name),
                                    ref ('lambda, 'lang, 'type_exp)
                                )
                            ),
                            'tuple -> record (
                                'elems -> apply (
                                    ref ('lambda, 'sdk, 'seq),
                                    ref ('lambda, 'lang, 'type_exp)
                                )
                            )
                        ),
                        'value -> taggedUnion ()
                    )
                )
            )
        )

}
