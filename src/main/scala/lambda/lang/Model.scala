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
                    'type_exp -> taggedUnion (
                        'type_const -> taggedUnion (
                            'module_type -> record (
                                'child_types -> apply (
                                    ref ('lambda, 'sdk, 'map),
                                    ref ('lambda, 'lang, 'name),
                                    ref ('lambda, 'lang, 'type_exp)
                                )
                            ),
                            'record_type -> record (
                                'fields -> apply (
                                    ref ('lambda, 'sdk, 'seq),
                                    tuple (
                                        ref ('lambda, 'lang, 'name),
                                        ref ('lambda, 'lang, 'type_exp)
                                    )
                                )
                            ),
                            'tagged_union_type -> record (
                                'sub_types -> apply (
                                    ref ('lambda, 'sdk, 'map),
                                    ref ('lambda, 'lang, 'name),
                                    ref ('lambda, 'lang, 'type_exp)
                                )
                            ),
                            'tuple_type -> record (
                                'elems -> apply (
                                    ref ('lambda, 'sdk, 'seq),
                                    ref ('lambda, 'lang, 'type_exp)
                                )
                            )
                        ),
                        'type_select -> record (
                            'scope -> ref ('lambda, 'lang, 'type_scope),
                            'path -> apply (
                                ref ('lambda, 'sdk, 'seq),
                                ref ('lambda, 'lang, 'name)
                            )
                        ),
                        'type_apply -> record (
                            'constructor -> ref ('lambda, 'lang, 'type_exp),
                            'args -> apply (
                                ref ('lambda, 'sdk, 'seq),
                                ref ('lambda, 'lang, 'type_exp)
                            )
                        )
                    ),
                    'type_scope -> taggedUnion (
                        'root_module -> record ()
                    )
                )
            )
        )

}
