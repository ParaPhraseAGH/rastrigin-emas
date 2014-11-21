#!/usr/bin/env sh

run () {

    for model in $models; do
        for run in `seq 1 $run_repeat`; do
            for ops in $operators; do
                for workers in $skel_workers; do

                    echo "running $model in $rtime mlsecs with $workers skel workers with $ops operators.."

                    dir=$output_root/$ops/$model/$run/$workers
                    mkdir -p $dir

                    logfile="emas_$rtime"`date +"-%s"`".log"
                    output_file=$dir/$logfile

                    ./rastrigin --time $rtime \
                                --model $model \
                                --skel_workers $workers \
                                --genetic_ops $ops \
                                --problem_size 100 \
                                --skel_split_size $split_size \
                        || exit 1

                    
                done
            done
        done
    done
}



output_dir="output"
rtime=5000

run_repeat=3
skel_workers=4
models="mas_sequential mas_skel mas_hybrid mas_concurrent"
operators="rastrigin_bin_ops rastrigin_nif_ops"

output_root=$output_dir/tests

run
